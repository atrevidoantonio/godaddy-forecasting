library(dplyr)
library(tidyverse)
library(tidymodels)
library(parsnip)
library(timetk)
library(modeltime)
library(sparklyr)
source("./scripts/forecast_helpers.R")

#' set up Apache Spark
#spark_install(version = "3.3")
#Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_361")
sc <- spark_connect(master = "local")
parallel_start(sc, .method = "spark")

#' data
train <- read_csv("./data/processed/enriched_train.csv") %>%
  mutate(month_label = month(date, label = TRUE),
         year = year(date))
test <- read_csv("./data/test.csv")
#' additional data
merged_jolts <- read_csv("./data/processed/merged_jolts.csv") %>% rename(sempl = empl)
#' convert to tsibble
train <-
  mutate(train, ym = yearmonth(date)) %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym) %>%
  fill_gaps() %>%
  select(-row_id)
#' impute missing activity data
train <-
  train %>%
  group_by(cfips) %>%
  imputeTS::na_locf()
#' join JOLTS data
train <- left_join(train, merged_jolts) %>%
  mutate(seekers = uneml*1000/openings)
#' fill missing COVID data to zero
train <- train %>%
  imputeTS::na_replace(0)

train_subset <-
  train %>%
  transmute(
    date,
    month_label = month(date, label = TRUE),
    cfips,
    county,
    state,
    activity,
    active,
    income_per_capita,
    population,
    unem = unem/100,
    gcr,
    discharges,
    separations,
    pct_bb,
    pct_college,
    pct_foreign_born
  )

train_subset <-
  train_subset %>%
  group_by(cfips) %>%
  mutate(
    trend = row_number()
    # activity_l1 = lag(activity),
    # activity_l2 = lag(activity, n = 2),
    # unem_l1 = lag(unem),
    # unem_l2 = lag(unem, n = 2)
  ) %>%
  ungroup()

train_end <- max(train$date)
test_end <- max(ymd(test$first_day_of_month))
dmonths <- floor(as.numeric(difftime(test_end, train_end, units = "weeks")) / 4)

#' modeltime workflow for fitting ARIMA and ARIMA with XGBOOST errors
nested_data_tbl <- train_subset %>%
  # 1. Extending: forecast 8 months
  extend_timeseries(
    .id_var        = cfips,
    .date_var      = date,
    .length_future = dmonths
  ) %>%
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 8 weeks of extended data and
  #    an actual dataset that contains 39 weeks
  nest_timeseries(
    .id_var        = cfips,
    .length_future = dmonths,
    .length_actual = 39
  ) %>%
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 36 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = 4,
    cumulative = TRUE
  )
#' ARIMA and Boosted ARIMA
rec <-
  recipe(
    activity ~ date + trend + pct_college + pct_bb + income_per_capita,
    extract_nested_train_split(nested_data_tbl)
  ) %>%
  step_log(income_per_capita)
#' Boosted ARIMAX
boosted_rec <-
  recipe(
    activity ~ date + pct_college + pct_bb,
    extract_nested_train_split(nested_data_tbl)
  )
#' XGBoost
rec_xgb <- recipe(activity ~ ., extract_nested_train_split(nested_data_tbl)) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
  add_model(
    boost_tree("regression") %>% set_engine(
      "xgboost",
      max_depth = 3,
      learn_rate = 0.05,
      subsample = 0.5,
      colsample_bytree = 0.5,
      counts = FALSE,
      gamma = 0.1,
      lambda = 1,
      nrounds = 5000,
      early_stopping_rounds = 50,
      tree_method  = "hist"
      )
  ) %>%
  add_recipe(rec_xgb)

wflw_arima <- workflow() %>%
  add_model(
    arima_boost(
      seasonal_period = 12,
      min_n = 1,
      tree_depth = 3,
      learn_rate = 0.1,
      sample_size = 0.5
    ) %>%
      set_engine(
        engine = "auto_arima_xgboost",
        nrounds = 5000,
        max_depth = 3,
        early_stopping_rounds = 50,
        colsample_bytree = 0.50,
        counts = FALSE
      )
  ) %>%
  add_recipe(boosted_rec)

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec)

mars_fit <- mars(mode = "regression") %>%
  set_engine("earth")

mars_rec <-
  recipe(
    activity ~ date + trend + month_label + pct_college + pct_bb + income_per_capita,
    extract_nested_train_split(nested_data_tbl)
  ) %>%
  step_normalize(trend) %>%
  step_rm(date)

wflw_mars <- workflow() %>%
  add_model(mars_fit) %>%
  add_recipe(mars_rec)

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  # Add workflows
  wflw_arima
)

nested_modeltime_tbl %>%
  extract_nested_test_accuracy()

nested_modeltime_tbl %>%
  extract_nested_error_report()

nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(verbose = TRUE)
  )

modeltime_fcts <-
  nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast()

arima_fcts <-
  filter(modeltime_fcts, .key == "prediction")

# xgboost_fcts <-
#   filter(modeltime_fcts, .model_desc == "XGBOOST", .key == "prediction") %>%
#   group_by(cfips) %>%
#   count()

# county_fcts <- left_join(counties, xgboost_fcts) %>%
#   filter(is.na(n)) %>%
#   select(-n) %>%
#   left_join(nested_modeltime_tbl)

arima_boost <-
  arima_fcts %>%
  transmute(
    row_id = paste(cfips, lubridate::ymd(.index), sep = "_"),
    microbusiness_density = .value
  )

filter(modeltime_fcts, .model_desc == "ARIMA W XGBOOST ERRORS", .key == "prediction")

save_data(arima_boost, path = "output")
spark_disconnect(sc)
