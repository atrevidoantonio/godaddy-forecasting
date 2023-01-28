df <-
  train_excluded %>% 
  group_split(cfips)
county_names <- train %>% as_tibble %>% distinct(cfips, county, state) %>%
  transmute(cfips, county = paste(county, state, sep = " "))
county_names <- unique(county_names$county)
#' name the elements of the list
names(df) <- county_names
#sort(unique(counties$cfips)) -> county_names
#' perform tests at the county level
dickey_fuller_tests <- lapply(df, unit_root_test) %>% plyr::ldply(tibble)
kpss_tests <- lapply(df, unit_root_test, test = "KPSS") %>% plyr::ldply(tibble)
pp_tests <- lapply(df, unit_root_test, test = "PP") %>% plyr::ldply(tibble)

bind_rows(dickey_fuller_tests, kpss_tests, pp_tests) %>%
  select(county = .id, p.value, method) %>%
  left_join(county_names) %>%
  relocate(cfips) %>%
  pivot_wider(names_from = method, 
              values_from = p.value)
