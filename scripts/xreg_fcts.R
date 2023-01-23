train_subset <-
  train %>%
  transmute(
    date,
    ym,
    cfips,
    county,
    state,
    census_region,
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
    pct_foreign_born,
    pct_it_workers,
    cases,
    cases_pop
  )

train_subset <-
  train_subset %>%
  group_by(cfips) %>%
  mutate(
    activity_l1 = lag(activity),
    activity_l2 = lag(activity, n = 2),
    unem_l1 = lag(unem),
    unem_l2 = lag(unem, n = 2),
    cases_l4 = lag(cases, n = 4),
    casepop_l4 = lag(cases_pop, n = 4)
  ) %>%
  ungroup()

train_subset <- train_subset %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym)

plan(multisession)

unem_fcts <- train_subset %>%
  model(stl = decomposition_model(STL(unem ~ trend(window = 21), robust = TRUE),
                                  ETS(season_adjust))) %>%
  forecast(h = dmonths)  %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, unem = .mean)

active_fcts <- train_subset %>%
  model(prophet = fable.prophet::prophet(
    log(active) ~ growth(type = "linear") + season(period = 12, type = "multiplicative")
  )) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  transmute(ym, cfips, state, census_region, active = round(.mean, digits = 0))

active_fcts <-
  imputeTS::na_interpolation(active_fcts)

cases_fcts <- train_subset %>%
  model(stl = decomposition_model(STL(cases_pop ~ trend(window = 21), robust = TRUE),
                                  ETS(season_adjust))) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, cases_pop = .mean)

case_fcts <- train_subset %>%
  model(stl = decomposition_model(STL(cases ~ trend(window = 21), robust = TRUE),
                                  ETS(season_adjust))) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, cases = .mean)

case_fcts <- mutate(case_fcts, cases = if_else(cases < 0, 0, as.numeric(cases)))

income_fct <- train_subset %>%
  model(tslm = TSLM(log(income_per_capita) ~ trend())) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, income_per_capita = .mean)

college_fct <- train_subset %>%
  model(theta = THETA(pct_college)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, pct_college = .mean)

for_fct <- train_subset %>%
  model(theta = THETA(pct_foreign_born)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(ym, cfips, state, census_region, pct_foreign_born = .mean)

fut_tbl <- left_join(income_fct, unem_fcts) %>%
  left_join(case_fcts) %>%
  left_join(cases_fcts) %>%
  left_join(active_fcts) %>%
  left_join(college_fct) %>%
  left_join(for_fct)

fut_tbl <- fut_tbl %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym)

train %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym, y = jdr)) +
  geom_step() +
  theme_clean() +
  facet_wrap(~cfips, scales = "free_y")

train %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = log(lag(discharges, n = 12)), y = log(active))) +
  geom_jitter() +
  theme_clean() +
  facet_wrap(~cfips, scales = "free")


wa_subset <- filter(train_subset, state == "Washington")

plan(multisession)

arima_fit <- train_subset %>%
  model(arima = ARIMA(
    sqrt(activity) ~ pct(pct_college) + sqrt(income_per_capita) + pct_bb, stepwise = FALSE, approximation = FALSE,
  ))

arima_fcts <- 
  arima_fit %>%
  forecast(new_data = fut_tbl)

tslm_fct <- format_fcts(tslm_fcts)
arima_fct <- format_fcts(arima_fcts)
save_data(arima_fct, path = "output")
save_data(tslm_fct, path = "output")