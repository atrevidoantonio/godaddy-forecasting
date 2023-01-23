#!/usr/bin/Rscript
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(scales)
library(tibbletime)
library(ForecastComb)
library(distributional)
library(ggdist)
library(future.apply)
library(future)
library(lubridate)
library(ggh4x)
library(hrbrthemes)
library(tsibble)
library(fable.prophet)
library(fable)
library(fabletools)
library(feasts)
library(seasonal)
library(zoo)

source("./scripts/forecast_helpers.R")

##### DATA #####
train <- read_csv("./data/processed/enriched_train.csv")
test <- read_csv("./data/test.csv")
#' additional data
covid_counties <- read_csv("./data/processed/covid_counties.csv")
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
#' #' join COVID data
#' train <- left_join(train, covid_counties)
#' join JOLTS data
train <- left_join(train, merged_jolts) %>%
  mutate(seekers = uneml*1000/openings)
#' fill missing COVID data to zero
train <- train %>%
  imputeTS::na_replace(0)

train_end <- max(train$date)
test_end <- max(ymd(test$first_day_of_month))
dmonths <-  floor(as.numeric(difftime(test_end, train_end, units = "weeks"))/4)


counties <- distinct(train, cfips, county, state, census_region)
ground_truth <- select(train, cfips, activity)

##### Feature Engineering ####

#' create lags for activity


train_hts <- train %>%
  aggregate_key((cfips * state) * census_region, active = sum(active)) %>%
  filter(!is_aggregated(census_region))

plan(multisession)

#' break up training data to observe accuracy
train_sample <- filter(train, date <= "2022-07-01")
test_sample <- filter(train, date > "2022-07-01") %>% select(cfips, activity)

#' Model fitting
#' Fable combination forecasting
fit <- 
  progressr::with_progress(
  train_sample %>%
  model(
    nmean = MEAN(activity),
    theta = THETA(activity),
    tslm = TSLM(activity ~ trend() + season(period = 12)),
    ets = ETS(activity ~ trend() + season(period = 12, method = "A") + error(method = "A")),
    arima = ARIMA(activity, stepwise = FALSE, approximation = FALSE),
    rw = ARIMA(activity ~ pdq(d = 1))
    )
  )

test_fits <- fit %>% fabletools::forecast(h = 3) %>%
  as_tibble() %>%
  mutate(model_type = "Forecast") %>%
  select(cfips, state, census_region, .model, ym, .fitted = .mean, model_type)

model_fits <-
  fit %>%
  fitted() %>%
  mutate(model_type = "Training") %>%
  bind_rows(test_fits) %>%
  left_join(ground_truth)

fits <- 
  model_fits %>%
  pivot_wider(names_from = .model,
            values_from = .fitted)

#' prepare for ForecastComb
actual <- ground_truth$activity
forecasts <- model_fits %>%
  as_tibble %>%
  select(ym, cfips, .model, .fitted) %>%
  pivot_wider(names_from = .model,
              values_from = .fitted) %>%
  select(arima:tslm) %>%
  as.matrix()

#' put together
input_data <- foreccomb(actual, forecasts)

#' combine models
fit <- mutate(fit, comb = (nmean + theta + tslm + ets + arima + rw)/6)

train_fcts <- fit %>% fabletools::forecast(h = dmonths) %>%
  hilo(level = c(80, 95)) %>%
  unpack_hilo(c("80%", "95%"), names_repair = fix_names)

#' refit models
model_refits <- fit %>%
  #' Theta method not currently supported for refitting
  select(-theta) %>%
  refit(train)
#' fit theta separately on full data set
theta <- train %>%
  model(theta = THETA(activity))

#' join back in theta model
model_refits <- left_join(model_refits, theta) %>%
  mutate(comb = (nmean + tslm + ets + arima + rw + theta)/6)

sigma2 <- model_refits %>% select(nmean:theta) %>% glance() %>% pull(sigma2)

model_tbl <-
  model_refits %>%
  select(-comb) %>%
  fitted() %>%
  as_tibble %>%
  mutate(lower_95 = .fitted - 1.96 * sqrt(sigma2),
         upper_95 = .fitted + 1.96 * sqrt(sigma2)) %>%
  left_join(ground_truth) %>%
  mutate(model_type = "Training")

nnetar_fit <-
  progressr::with_progress(train %>%
                             model(nnetar = NNETAR(activity)))
arima_fit <- train %>%
  model(arima = ARIMA(activity ~ PDQ(0, 1, 0, period = 12), stepwise = FALSE))

#' produce point forecasts
fcts <-
  model_refits %>%
  forecast(h = dmonths)

#' Monte Carlo simulation to produce probabilistic forecasts
futures <-
  model_refits %>%
  generate(h = dmonths, times = 1000) %>%
  # Compute forecast distributions from future sample paths
  as_tibble() 
  group_by(ym, .model) %>%
  summarise(
    dist = distributional::dist_sample(list(.sim))
  ) %>%
  ungroup()
  # Create fable object
  as_fable(index = ym, key = .model,
           distribution = dist, response = "activity")

  
#' These steps should all be put into a single function
#' prepare for ForecastComb
actual <- ground_truth$activity

train_fit <- model_refits %>%
  fitted() %>%
  as_tibble %>%
  select(ym, cfips, .model, .fitted) %>%
  pivot_wider(names_from = .model,
              values_from = .fitted) %>%
  select(nmean:theta) %>%
  as.matrix()

model_fcts <- fcts %>%
  as_tibble %>%
  select(ym, cfips, .model, .mean) %>%
  pivot_wider(names_from = .model,
              values_from = .mean) %>%
  select(nmean:theta) %>%
  as.matrix()
  
#' put together
input_data <-
  foreccomb(
    observed_vector = actual,
    prediction_matrix = train_fit,
    newpreds = model_fcts
  )
#' utilize auto_combine to find the best combination model
comb_fct <- ForecastComb::auto_combine(input_data)

comb_fit <-
  train %>%
  select(cfips, state, census_region, ym, activity) %>%
  mutate(model_type = "Training",
         .model = "comb",
         .fitted = comb_fct$Fitted)

model_tbl <-
  bind_rows(model_tbl, comb_fit) %>%
  arrange(cfips, .model, ym)

fct_probs <-
  fcts %>%
  hilo(level = 95) %>% 
  unpack_hilo("95%", names_repair = fix_names)

#' final forecast table
fct_tbl <- fcts %>%
  as_tibble %>%
  select(ym, cfips, .model, .mean) %>%
  pivot_wider(names_from = .model,
              values_from = .mean) %>%
  mutate(comb_auto = comb_fct$Forecasts_Test)

fct_values <-
  fct_probs %>%
  rename(.fitted = .mean,  lower_95 = `95_lower`, upper_95 = `95_upper`) %>%
  as_tibble() %>%
  mutate(activity = NA) %>%
  mutate(model_type = "Forecast") %>%
  bind_rows(model_tbl) %>%
  arrange(cfips, .model, ym)

submission <-
  select(fct_tbl, ym, cfips, microbusiness_density = comb_auto) %>%
  mutate(first_day_of_month = lubridate::ym(ym)) %>%
  transmute(row_id = paste(cfips, first_day_of_month, sep = "_"),
            microbusiness_density)

nnetar_fcts <-
  progressr::with_progress(
  nnetar_fit %>%
  forecast(h = dmonths) %>%
  as_tibble() %>%
  transmute(
    row_id = paste(cfips, lubridate::ym(ym), sep = "_"),
    microbusiness_density = .mean)
  )
theta_fcts <-
  fcts %>%
  as_tibble() %>%
  filter(.model == "theta") %>%
  transmute(
    row_id = paste(cfips, lubridate::ym(ym), sep = "_"),
    microbusiness_density = .mean
  )

save_data(ets_fcts, path = "output")
save_data(theta_fcts, path = "output")
save_data(tslm_fcts, path = "output")
save_data(arima_fcts, path = "output")
save_data(fct_values, path = "output")
save_data(submission, path = "output")

#' save training models
save_model(model_refits)
save_model(ets_fit)
save_model(tslm_fit)
save_model(theta_fit)
save_model(arima_fit)
save_model(comb_fct)