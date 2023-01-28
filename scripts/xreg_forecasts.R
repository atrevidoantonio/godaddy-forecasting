
library(dplyr)
library(tidyverse)
library(fable)

xreg <- c("pct_bb", "pct_college", "income_per_capita", "pct_foreign_born")

pct_bb <- train %>%
  model(fit = NAIVE(pct_bb)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(cfips, ym, pct_bb = .mean)

pct_college <- train %>%
  model(fit = NAIVE(pct_college)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(cfips, ym, pct_college = .mean)

income_per_capita <- train %>%
  model(fit = NAIVE(income_per_capita)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(cfips, ym, income_per_capita = .mean)

pct_foreign_born <- train %>%
  model(fit = NAIVE(pct_foreign_born)) %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  select(cfips, ym, pct_foreign_born = .mean)

xreg_tbl <- bind_cols(pct_bb, pct_college, income_per_capita, pct_foreign_born)

xreg_tbl <-
  xreg_tbl %>%
  rename(cfips = cfips...1,
         ym = ym...2) %>%
  select(-starts_with("ym...")) %>%
  select(-starts_with("cfips..."))

unem_dcp <- decomposition_model(STL(log(unem) ~ season(window = 21)),
                                ETS(season_adjust ~ season("N")),
                                SNAIVE(season_year))
unem_fcts <-
  train %>%
  model(unem_dcp) %>% 
  forecast(h = dmonths)

unem_fcts <-
  unem_fcts %>%
  as_tibble() %>%
  select(cfips, ym, unem = .mean)

xregs <- left_join(xreg_tbl, unem_fcts)

xregs <-
  xregs %>%
  left_join(counties) %>%
  relocate(ym, cfips, county, state, census_region) %>%
  as_tsibble(key = c(cfips, county, state, census_region), index = ym)
