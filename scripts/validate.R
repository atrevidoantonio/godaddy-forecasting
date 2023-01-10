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

arima_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth) %>%
  yardstick::metrics(activity, .fitted)

theta_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth) %>%
  yardstick::metrics(activity, .fitted)


Metrics::smape(ets_train_fit$activity, ets_train_fit$.fitted)
Metrics::smape(tslm_train_fit$activity, tslm_train_fit$.fitted)
Metrics::smape(arima_train_fit$activity, arima_train_fit$.fitted)

Metrics::smape(ets_test_fit$activity, ets_test_fit$.fitted)
yardstick::metrics(ets_test_fit, activity, fct)