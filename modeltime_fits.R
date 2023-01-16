library(parsnip)
library(modeltime)

wa_train <- train %>%
  filter(state == "Washington") %>%
  as_tibble() %>%
  select(cfips, activity) %>%
  group_split(cfips)

wa_train <-
  wa_train %>% map(
    .f = function(x) {
      x %>%
        select(activity) %>%
        ts(
          start = as.Date("2019-08-01"),
          end = as.Date("2022-10-01"),
          frequency = 12
        )
    }
  )

arima_fit <- function(data){
   fit <- forecast::auto.arima(data, stationary = TRUE)
   return(fit)
}

arima_fit <- function(data){
  ts_data <- ts(train$activity, freq = 12)
  model <- forecast::auto.arima(ts_data, stationary = TRUE, biasadj = TRUE)
  return(model)
}

nested_tbl <- plyr::l_ply(wa_train, arima_fit, .progress = "text")

#' 
wa_train <- filter(train_sample, state == "Washington")
wa_test <- filter(train, state == "Washington", date > "2022-07-01") %>% as_tibble()
wa_full <- filter(train, state == "Washington") %>% as_tibble()

nested_tbl <-
  wa_train <- filter(train_sample, state == "Washington") %>%
  group_by(cfips) %>%
  nest()

#' modeltime workflow for fitting ARIMA and ARIMA with XGBOOST errors
nested_data_tbl <- wa_full %>%
  # 1. Extending: We'll predict 12 months into the future.
  extend_timeseries(
    .id_var        = cfips,
    .date_var      = date,
    .length_future = 12
  ) %>%
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 12 weeks of extended data and
  #    an actual dataset that contains 39 weeks
  nest_timeseries(
    .id_var        = cfips,
    .length_future = 12,
    .length_actual = 39
  ) %>%
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 36 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = 4,
    cumulative = TRUE
  )

rec <- recipe(activity ~ date, extract_nested_train_split(nested_data_tbl))

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
  wkfl_arima,
  wkfl_prophet
)

nested_modeltime_tbl %>% 
  extract_nested_test_accuracy()

nested_modeltime_refit_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(verbose = TRUE)
  )

nested_modeltime_tbl %>%
  extract_nested

modeltime_fits <-
  nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast()

nested_modeltime_refit_tbl %>%
  extract()
