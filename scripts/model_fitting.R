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
      strip.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.ticks.length = unit(0.5, "cm"),
      axis.ticks = element_line(size = 0.15)
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

sapphire <- "#255F85"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
raspberry <- "#C33149"
dark_violet <- "#351C45"
satin <- "#D65C70"
onyx <- "#383C42"
rhythm <- "#6E758E"
gainsboro <- "#D8D8D8"
persian_orange <- "#DC965A"
metallic <- "#837A75"
slate <- "#3f3f3f"
charcoal <- "#2E4057"
claret <- "#8B1E3F"
dark_magenta <- "#861B54"
eton <- "#739F8F"
dark_emerald <- "#2d6d66"
##### DATA #####

train <- read_csv("./data/processed/enriched_train.csv")
test <- read_csv("./data/test.csv")

ts <-
  mutate(train, ym = yearmonth(date)) %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym) %>%
  fill_gaps() %>%
  select(-row_id)

ground_truth <- select(ts, cfips, activity)

plan(multisession)

tslm_fit <- ts %>%  model(tslm = TSLM(activity ~ trend() + season()))
ets_fit <- ts %>%  model(ets = ETS(activity ~ trend()))
arima_fit <- ts %>% model(arima = ARIMA(activity))

#PCA
xreg <- train %>% select(activity:pct_it_workers)
pca <- xreg %>% na.omit() %>% prcomp(xreg)

#' fable ensemble forecasting
fit <- ts %>%
  model(
    snaive = SNAIVE(activity ~ lag("year")),
    tslm = TSLM(activity ~ trend()),
    ets = ETS(activity),
    ar  = AR(activity),
    rw = ARIMA(activity ~ pdq(d = 1)),
    arima = ARIMA(activity, stepwise = FALSE, approximation = FALSE)
  ) %>% 
  mutate(ensemble = (snaive + tslm + ets + ar + rw + arima) / 6)

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


tslm_train_fit <-
  tslm_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth)

ets_train_fit <-
  ets_fit %>%
  fitted() %>%
  as_tibble() %>%
  left_join(ground_truth)

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