library(tidymodels)
library(parsnip)
library(timetk)
library(modeltime)
source("./scripts/forecast_helpers.R")

df <- read_csv("./data/processed/enriched_train.csv") %>%
  mutate(month_label = month(date, label = TRUE),
         year = year(date))
  
#' modeltime workflow for fitting ARIMA and ARIMA with XGBOOST errors
nested_data_tbl <- df %>%
  # 1. Extending: forecast 8 months
  extend_timeseries(
    .id_var        = cfips,
    .date_var      = date,
    .length_future = 8
  ) %>%
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 8 weeks of extended data and
  #    an actual dataset that contains 39 weeks
  nest_timeseries(
    .id_var        = cfips,
    .length_future = 8,
    .length_actual = 39
  ) %>%
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 36 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = 4,
    cumulative = TRUE
  )

rec <-
  recipe(
    activity ~ date + year + month_label,
    extract_nested_train_split(nested_data_tbl)
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wkfl_arima <- workflow() %>%
  add_model(
    arima_boost(
      seasonal_period = 12,
      min_n = 1,
      learn_rate = 0.1
    ) %>%
      set_engine(engine = "auto_arima_xgboost")
  ) %>%
  add_recipe(rec)

wkfl_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec)

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  # Add workflows
  wkfl_arima
)

nested_modeltime_tbl %>%
  extract_nested_test_accuracy()

nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(verbose = TRUE)
  )

modeltime_fits <-
  nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast()

arima_boost <- filter(modeltime_fits, .key == "prediction") %>%
  transmute(
    row_id = paste(cfips, lubridate::ymd(.index), sep = "_"),
    microbusiness_density = .value
  )

save_data(arima_boost, path = "output")
