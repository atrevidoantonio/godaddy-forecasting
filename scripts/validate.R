tslm_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth) %>%
  yardstick::metrics(activity, .fitted)

ets_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth) %>%
  yardstick::metrics(activity, .fitted)

tslm_train_fit <-
  fit %>%
  select(tslm) %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth)

model_fits %>%
  as_tibble %>%
  group_by(.model) %>%
  yardstick::metrics(activity, .fitted) %>%
  filter(.metric == "mae") 


smape <- function(actual, forecast) {
  return(1 / length(actual) * sum(2 * abs(forecast - actual) / (abs(actual) + abs(forecast)), na.rm = TRUE))
}

Metrics::smape(ets_train_fit$activity, ets_train_fit$.fitted)
Metrics::smape(tslm_train_fit$activity, tslm_train_fit$.fitted)
smape(arima_train_fit$activity, arima_train_fit$.fitted)
Metrics::smape(arima_train_fit$activity, arima_train_fit$.fitted)

Metrics::smape(ets_test_fit$activity, ets_test_fit$.fitted)
yardstick::metrics(ets_test_fit, activity, fct)