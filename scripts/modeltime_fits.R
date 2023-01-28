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
test <- read_csv("./data/test.csv") %>%
  transmute(row_id, cfips, date = ymd(first_day_of_month))
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

#' Explanatory variables (Feature engineering for ML models)
train_subset <- train_subset %>% 
  group_by(cfips) %>%
  mutate(
    mbd_lag_1 = lag(activity),
    mbd_lag_2 = lag(activity, n = 2L),
    mbd_lag_3 = lag(activity, n = 3L),
    act_lag_1 = active - lag(active),
    act_lag_2 = active - lag(active, n = 2L),
    act_lag_3 = active - lag(active, n = 3L),
    mbd_rollmean2 = zoo::rollsum(mbd_lag_1, 2, fill = NA),
    mbd_rollmean4 = zoo::rollsum(mbd_lag_1, 4, fill = NA),
    mbd_rollmean6 = zoo::rollsum(mbd_lag_1, 6, fill = NA),
    mbd_rollmean12 = zoo::rollsum(mbd_lag_1, 12, fill = NA)
  ) %>%
  ungroup()

features <-
  c(
    "state_dummy",
    "mbd_lag_1",
    "mbd_lag_2",
    "mbd_lag_3",
    "act_lag_1",
    "act_lag_2",
    "act_lag_3" ,
    "mbd_rollmean2",
    "mbd_rollmean4",
    "mbd_rollmean6",
    "mbd_rollmean12"
  )  

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
    activity ~ date,
    extract_nested_train_split(nested_data_tbl)
  )
#' Boosted ARIMA
boosted_rec <-
  recipe(
    activity ~ date  + month_label,
    extract_nested_train_split(nested_data_tbl)
  )
#' XGBoost
rec_xgb <- recipe(activity ~ ., extract_nested_train_split(nested_data_tbl)) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

#' XGBoost model needs refinement,
#' some counties are not inexplicably dropped from the forecast at the end
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
      tree_depth = 3,
      learn_rate = 0.1
    ) %>%
      set_engine(
        engine = "auto_arima_xgboost",
        nrounds = 5000,
        max_depth = 3,
        early_stopping_rounds = 50
      )
  ) %>%
  add_recipe(boosted_rec)

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec)


ardl_fit <- auto_ardl(activity ~ active + discharges + pct_college + pct_bb, max_order = 5, data = wa_subset)

nested_df <- train_subset %>%
  group_by(cfips) %>%
  nest()

# ---- AUTO ADAM ----
# Model Spec
model_spec <- adam_reg() %>%
  set_engine("auto_adam")
d

fit_adam <- function(tsdata, prop = 0.9, h = 8, forecast = TRUE) {
  splits <-
    initial_time_split(tsdata, prop = prop, cumulative = TRUE)
  adam_fit <- model_spec %>%
    fit(activity ~ date, data = training(splits)) %>%
    modeltime_table() %>%
    modeltime_calibrate(new_data = testing(splits))
  if (forecast) {
    adam_fcts <- modeltime_refit(adam_fit, data = tsdata) %>%
      modeltime_forecast(h = h,
                         actual_data = tsdata)
    return(adam_fcts)
  }
}

# adam_fcts <- lapply(nested_df, fit_adam)
# plyr::ldply(adam_fcts, as_tibble)

nested_df <- nested_df %>%
  mutate(forecast = map(data, ~fit_adam(.x, forecast = TRUE)))

adam_fcts <-
  unnest(nested_df, c(cfips, forecast)) %>%
  select(cfips, date = .index, .model_desc, .value)

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  # Add workflows
  wflw_adam
)

nested_modeltime_tbl %>%
  extract_nested_test_accuracy()

nested_modeltime_tbl %>%
  extract_nested_error_report()

nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit()
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

arima_boost <-
  arima_fcts %>%
  transmute(
    row_id = paste(cfips, lubridate::ymd(.index), sep = "_"),
    microbusiness_density = .value
  )

save_data(arima_boost, path = "output")
spark_disconnect(sc)
