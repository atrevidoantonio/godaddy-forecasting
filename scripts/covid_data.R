covid_counties <- mutate(diffed, ym = yearmonth(date)) %>%
  group_by(ym, fips) %>%
  summarize(cases = sum(cases),
            pop = max(pop)) %>%
  arrange(fips, ym) %>%
  group_by(fips) %>%
  mutate(cases_pop = cases/pop*100,
           cuml_cases = cumsum(cases)) %>%
  ungroup() %>%
  transmute(date = lubridate::ym(ym),
            ym,
            cfips = sub("^0+", "", fips),
            cases,
            cases_pop
            )

save_data(covid_counties, path = "processed")

covid_counties <- mutate(covid_counties, cfips = as.numeric(cfips))

ground_truth %>% left_join(covid_counties)
