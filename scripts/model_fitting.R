#!/usr/bin/Rscript
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(future)
library(ggpubr)
library(ggsci)
library(scales)
library(tibbletime)
library(distributional)
library(ggdist)
library(lubridate)
library(ggh4x)
library(hrbrthemes)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)
library(feasts)
library(seasonal)
library(future.apply)
library(zoo)
library(forecast)

##### Helper Functions #####

unit_root_test <- function(tbl, test = "ADF") {
  #' custom function to implement several flavors of unit root tests for stationary time series
  #' defaults to the Augmented Dickey-Fuller test,but supports Kwiatkowski–Phillips–Schmidt–Shin (KPSS)
  #' and Phillips-Peron tests. Please note that for the KPSS test
  #' the presence of a unit root is not the null hypothesis but the alternative.
  if (test == "ADF") {
    result = tseries::adf.test(tbl$activity)
    result = tidy(result)
    return(result)
  }
  else if (test == "KPSS") {
    result = feasts::unitroot_kpss(tbl$activity)
    result = tidy(result) %>%
      pivot_wider(names_from = names, values_from = x) %>%
      rename(statistic = kpss_stat, p.value = kpss_pvalue) %>%
      mutate(method = "Kwiatkowski–Phillips–Schmidt–Shin Test",
             alternative = "not stationary")
    return(result)
  }
  else if (test == "PP") {
    result = feasts::unitroot_pp(tbl$activity)
    result = tidy(result) %>%
      pivot_wider(names_from = names, values_from = x) %>%
      rename(statistic = pp_stat, p.value = pp_pvalue) %>%
      mutate(method = "Phillips–Perron Test",
             alternative = "stationary")
    return(result)
  }
  else
    (print("Please specify a unit root test to perform!"))
}

###### Aesthetics #######
theme_clean <- function(...) {
  ggpubr::theme_classic2() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.15, colour = onyx),
      legend.title = element_text(size = 10, face = "plain"),
      legend.key = element_rect(fill = NA, color = NA),
      text = element_text(color = onyx, family = "Roboto Condensed"),
      strip.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      axis.ticks.length = unit(0.5, "cm"),
      axis.ticks = element_line(linewidth = 0.15)
    )
}

# grid style ggplot theme
theme_grid <- function(...) {
  theme_light() +
    theme(
      legend.background = element_rect(color = NA),
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      panel.border = element_blank(),
      legend.title = element_text(size = 10, face = "plain"),
      text = element_text(family = "Roboto Condensed", color = charcoal),
      plot.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(colour = 'black')
    )
}

classic_blue <- "#0F4C81"
sapphire <- "#255F85"
sky_blue <- "#6596CD"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
pink <- "#EFAAC4"
raspberry <- "#C33149"
violet <- "#5f4b8b"
dark_violet <- "#351C45"
onyx <- "#383C42"
dim_gray <- "#6B717E"
gainsboro <- "#D8D8D8"
metallic <- "#837A75"
slate <- "#3f3f3f"
charcoal <- "#2E4057"
claret <- "#8B1E3F"
pepper <- "#9b1b30"
dark_magenta <- "#861B54"
eton <- "#739F8F"
emerald <- "#00997b"
dark_emerald <- "#2d6d66"
##### DATA #####

#' regular expression to get forecast intervals in numeric format
fix_names <- function(x) gsub("[\\%,]", "", x)

get_forecasts <- function(mbl, h = 4, intervals = TRUE) {
  if (intervals) {
    fct <- forecast(mbl, h = h) %>%
      hilo(level = c(80, 95)) %>%
      unpack_hilo(c("80%", "95%"), names_repair = fix_names)
  } else {
    fct <- forecast(mbl, h = h)
  }
  return(fct)
}

training_fit <- function(model_fit) {
  df <- fitted(model_fit) %>%
    as_tibble() %>%
    select(-.model) %>%
    left_join(ground_truth) %>%
    mutate(fit = "Training")
  return(df)
}

test_fit <- function(model_fit) {
  fcts <- get_forecasts(model_fit)
  test_fits <- fcts %>%
    select(cfips, state, census_region, .mean, `95_lower`, `95_upper`) %>%
    rename(.fitted = .mean) %>%
    as_tibble() %>%
    relocate(ym, .after = census_region) %>%
    left_join(test_sample) %>%
    filter(!is.na(activity)) %>%
    mutate(fit = "Test")
  return(test_fits)
}

combine_model_fits <- function(model_fit, forecasts) {
  model_train_fit <-
    model_fit %>%
    fitted() %>%
    as_tibble() %>%
    select(-.model) %>%
    left_join(ground_truth) %>%
    mutate(fit = "Training")
  
  model_test_fit <-
    forecasts %>%
    select(cfips, state, census_region, .mean, `95_lower`, `95_upper`) %>%
    rename(.fitted = .mean) %>%
    as_tibble() %>%
    relocate(ym, .after = census_region) %>%
    left_join(test_sample) %>%
    filter(!is.na(activity)) %>%
    mutate(fit = "Forecast")
  df <- bind_rows(model_train_fit, model_test_fit)
  return(df)
}

train <- read_csv("./data/processed/enriched_train.csv")
test <- read_csv("./data/test.csv")

train <-
  mutate(train, ym = yearmonth(date)) %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym) %>%
  fill_gaps() %>%
  select(-row_id)

counties <- distinct(train, cfips, county, state, census_region)

ground_truth <- select(train, cfips, activity)

plan(multisession)
#' break up training data to observe accuracy
train_sample <- filter(train, date <= "2022-07-01")
test_sample <- filter(train, date > "2022-07-01") %>% select(cfips, activity)

#' Model fitting
tslm_fit <- train_sample %>%  model(tslm = TSLM(activity ~ trend() + season(period = 12)))
ets_fit <- train_sample %>%  model(ets = ETS(activity ~ trend() + season(period = 12, method = "A") + error(method = "A")))
theta_fit <- train_sample %>% model(theta = THETA(activity ~ season(period  = 12)))

#' Forecasts
tslm_forecasts <- get_forecasts(tslm_fit, h = 8)
ets_forecasts <- get_forecasts(ets_fit, h = 8)
theta_forecasts <- get_forecasts(theta_fit, h = 8)
arima_fit <- ts %>% model(arima = ARIMA(activity))

#PCA
xreg <- train %>% select(activity:pct_it_workers)
pca <- xreg %>% na.omit() %>% prcomp(xreg)

#' Fable ensemble forecasting
fit <- train %>%
  model(
    snaive = SNAIVE(activity ~ lag("year")),
    tslm = TSLM(activity ~ trend() + season(period = 12)),
    ets = ETS(activity ~ trend() + season(method = "A") + error(method = "A")),
    ar  = AR(activity),
    rw = ARIMA(activity ~ pdq(d = 1))
  ) %>% 
  mutate(ensemble = (snaive + tslm + ets + ar + rw + arima) / 5)

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

tslm_train_fit <-
  tslm_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth)

ets_train_fit <-
  ets_fit %>%
  fitted() %>%
  as_tibble() %>%
  select(-.model) %>%
  left_join(ground_truth) %>%
  mutate(fit = "Training")


ets_test_fit <-
  ets_forecasts %>%
  select(cfips, state, census_region, .mean, `95_lower`, `95_upper`) %>%
  rename(.fitted = .mean) %>%
  as_tibble() %>%
  relocate(ym, .after = census_region) %>%
  left_join(test_sample) %>%
  filter(!is.na(activity)) %>%
  mutate(fit = "Test")

ets_model <- combine_model_fits(ets_fit, ets_forecasts)
tslm_model <- combine_model_fits(tslm_fit, tslm_forecasts)
theta_model <- combine_model_fits(theta_fit, theta_forecasts)

arima_train_fit <- 
  arima_fit %>%
  fitted() %>%
  as_tibble() %>%
  select(-.model) %>%
  left_join(ground_truth) %>%
  mutate(fit = "Training")


ets_test_fit <-
  ets_forecasts %>%
  select(cfips, state, census_region, .mean, `95_lower`, `95_upper`) %>%
  rename(.fitted = .mean) %>%
  as_tibble() %>%
  relocate(ym, .after = census_region) %>%
  left_join(test_sample) %>%
  filter(!is.na(activity)) %>%
  mutate(fit = "Test")

ets_model <- combine_model_fits(ets_fit, ets_forecasts)
tslm_model <- combine_model_fits(tslm_fit, tslm_forecasts)
theta_model <- combine_model_fits(theta_fit, theta_forecasts)

arima_train_fit <- 
  arima_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth)

Metrics::smape(ets_train_fit$activity, ets_train_fit$.fitted)
Metrics::smape(tslm_train_fit$activity, tslm_train_fit$.fitted)



Metrics::smape(arima_train_fit$activity, arima_train_fit$.fitted)

stl_dcmp <- ts %>%
  model(STL(
    activity ~ trend(window = 21) +
      season(window = "periodic"),
    robust = TRUE)
  ) %>%
  components()
model
Metrics::smape(ets_test_fit$activity, ets_test_fit$.fitted)
yardstick::metrics(ets_test_fit, activity, fct)

train %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_line(aes(y = activity)) + 
  facet_wrap(vars(cfips), scales = "free") +
  theme_clean()

ets_train_fit %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_jitter(aes(y = activity, color = "Actual")) + 
  geom_line(aes(y = .fitted, color = "Forecast")) + 
  theme_clean() +
  labs(x = "", y = "Activity") +
  facet_wrap(vars(cfips), scales = "free")


ets_model %>%
  left_join(counties) %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")

ets_model %>%
  left_join(counties) %>%
  filter(state == "Oregon") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")

tslm_model %>%
  left_join(counties) %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")
  
ggsave("washington_small_multiples.svg", plot = last_plot(), dpi = 720, height = 8, width = 12)

ets_refit <- refit(ets_fit, train) %>%
  get_forecasts(h = 8)

fct <- ets_refit %>%
  select(ym:census_region, activity = .mean, lower = `95_lower`, upper = `95_upper`) %>%
  select(-.model) %>%
  mutate(model = "Forecast") %>%
  bind_rows(ground_truth) %>%
  arrange(cfips, ym) %>%
  mutate(model = if_else(is.na(model), "Current", as.character(model)),
         date = lubridate::ym(ym) %>% floor_date(., unit = "month")) %>%
  left_join(counties)

fct %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = upper, ymax = lower, fill = "95% CI"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Current")) +
  #' hacky work around since use color = model, splits the lines
  geom_line(data = fct %>% filter(state == "Washington", model == "Forecast"), aes(y = activity, color = "Forecast")) +
  theme_clean() +
  scale_color_manual(values = c(sky_blue, rainy_day)) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(~county, scales = "free")
