county_pop <- group_by(train, cfips) %>%
  summarize(population = round(mean(population)),
            active = round(mean(active, na.rm = TRUE)))

arima_fitted <- arima_fit %>%
  fitted() %>%
  left_join(ground_truth)

theta_fitted <- theta_fit %>%
  fitted() %>%
  left_join(ground_truth)

train_error <-
  arima_fitted %>%
  as_tibble() %>%
  mutate(model_type = "Training") %>%
  group_by(cfips, county, state, census_region, model_type) %>%
  rename(fitted = .fitted) %>%
  summarize(smape = yardstick::smape_vec(activity, fitted))

theta_train_error <-
  theta_fitted %>%
  as_tibble() %>%
  mutate(model_type = "Training") %>%
  group_by(cfips, county, state, census_region, model_type) %>%
  rename(fitted = .fitted) %>%
  summarize(smape = yardstick::smape_vec(activity, fitted)) 

test_fits <- arima_fit %>% fabletools::forecast(h = 3) %>%
  as_tibble() %>%
  mutate(model_type = "Forecast") %>%
  select(cfips, county, state, census_region, .model, ym, .fitted = .mean, model_type)

test_fits <- test_fits %>%
  select(-activity) %>%
  left_join(ground_truth)

theta_forecast_error <- theta_fit %>% fabletools::forecast(h = 3) %>%
  as_tibble() %>%
  mutate(model_type = "Forecast") %>%
  select(cfips, county, state, census_region, .model, ym, .mean, model_type) %>%
  left_join(ground_truth) %>%
  group_by(cfips, county, state, census_region, model_type) %>%
  summarize(smape = yardstick::smape_vec(activity, .mean))

test_fcts <- arima_fit %>% fabletools::forecast(h = 3) %>%
  hilo() %>%
  unpack_hilo(c("80%", "95%"), names_repair = fix_names) %>%
  as_tibble() %>%
  mutate(model_type = "Forecast") %>%
  select(-c(.model, activity))

high_error <-
  bind_rows(arima_fitted, test_fcts) %>%
  mutate(model_type = if_else(is.na(model_type), "Training", "Forecast")) %>%
  as_tibble() %>%
  select(-c(.model, activity)) %>%
  left_join(ground_truth) %>%
  filter(cfips %in% exluded_counties)

test_fits <- test_fits %>%
  select(-activity) %>%
  left_join(ground_truth)

forecast_error <- test_fits %>%
  group_by(cfips, county, state, census_region, model_type) %>%
  summarize(smape = yardstick::smape_vec(activity, .mean))

error_tbl <- bind_rows(train_error, forecast_error) %>%
  left_join(county_pop) %>%
  pivot_wider(names_from = model_type,
              values_from = smape) %>%
  janitor::clean_names()


theta_error <- bind_rows(train_error, theta_forecast_error) %>%
  left_join(county_pop) %>%
  pivot_wider(names_from = model_type,
              values_from = smape) %>%
  janitor::clean_names() %>%
  ungroup()

error_tbl %>% ungroup() %>% summarize(forecast = mean(forecast, na.rm = TRUE))
theta_error %>% ungroup() %>% summarize(forecast = mean(forecast))

county_naive <- filter(theta_error, forecast > 0.18) %>%
  distinct(cfips, county, state, census_region, forecast)

excluded_counties <- unique(county_naive$cfips)

train_subsample <- filter(train_sample, !cfips %in% excluded_counties)

train_excluded <-
  select(
    train,
    ym,
    cfips,
    county,
    state,
    census_region,
    activity,
    active,
    population,
    pct_bb,
    pct_college,
    income_per_capita,
    pct_foreign_born,
    unem,
    separations
  ) %>%
  mutate(excluded = if_else(cfips %in% excluded_counties, 1, 0))



nnetar_fit <- train_subsample %>%
  model(nnetar = NNETAR(activity))

nnetar_test_fits <- nnetar_fit %>%
  forecast(h = 3)

error_fcts <- filter(train, cfips %in% excluded_counties) %>%
  model(naive = NAIVE(activity)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  transmute(cfips, county, state, census_region, ym, microbusiness_density = .mean)

arima_fcts <- refit(arima_fit, train) %>% forecast(h = dmonths)
#' theta method has no refitting procedure in Fable
theta_fcts <- model(train, THETA(activity)) %>% forecast(h = dmonths)

#' exclude the counties from the ARIMA forecasts
arima_fct <-
  arima_fcts %>%
  filter(!cfips %in% excluded_counties) %>%
  as_tibble() %>%
  transmute(cfips, county, state, census_region, ym, microbusiness_density = .mean)
#' exclude the counties from the THETA forecasts
theta_fct <-
  theta_fcts %>%
  filter(!cfips %in% excluded_counties) %>%
  as_tibble() %>%
  transmute(cfips, county, state, census_region, ym, microbusiness_density = .mean)

arima_naive <- bind_rows(arima_fct, error_fcts) %>%
  arrange(cfips) %>%
  transmute(row_id = paste(cfips, lubridate::ym(ym), sep = "_"), microbusiness_density)

theta_naive <- bind_rows(theta_fct, error_fcts) %>%
  arrange(cfips) %>%
  transmute(row_id = paste(cfips, lubridate::ym(ym), sep = "_"), microbusiness_density)

save_data(arima_naive, path = "output")
save_data(theta_naive, path = "output")
# error_fcts %>%
#   as_tibble() %>%
#   select(-c(.model, activity, model_type)) %>%
#   rename(arima = .mean) %>%
#   left_join(ground_truth) %>%
#   summarize(naive = yardstick::smape_vec(activity, naive),
#             arima = yardstick::smape_vec(activity, arima))
