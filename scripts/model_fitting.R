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

#' regular expression to clean forecast intervals
fix_names <- function(x) gsub("[\\%,]", "", x)

#' Extract forecasts from a fitted model
#' @param mbl A fitted model object
#' @param h The number of forecast periods
#' @param intervals A Boolean indicating whether to include prediction intervals
#' @return A tibble with the forecast data
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

#' Extract and format training data fit:
#' @param model_fit A fitted model object
#' @return A tibble with the training data fit
#' @examples
#' model_fit <- lm(y ~ x)
#' training_fit(model_fit)
training_fit <- function(model_fit) {
  df <- fitted(model_fit) %>%
    as_tibble() %>%
    select(-.model) %>%
    left_join(ground_truth) %>%
    mutate(fit = "Training")
  return(df)
}
#' Extract and format test data fit
#' @param model_fit A fitted model object
#' @return A tibble with the test data fit
#' @examples
#' model_fit <- lm(y ~ x)
#' test_fit(model_fit)
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
#' combine_model_fits takes two arguments:
#' @param model_fit, which is a model object fit on training data
#' and @param forecast' the forecasted values from the
#' model fit. This function combines the fitted values of the training fit
#' and the forecast into a single dataframe along with the ground truth data
combine_model_fits <- function(model_fit, forecasts) {
  #' training fit of the model 
  model_train_fit <-
    model_fit %>%
    fitted() %>%
    as_tibble() %>%
    select(-.model) %>%
    left_join(ground_truth) %>%
    mutate(fit = "Training")
#' test fit
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

#' save_model takes two arguments:
#'@param model_fit, the fitted model object be saved. 
#' The function will then use the deparse and substitute functions
#' to extract the name of the object passed as an argument and use it as the filename for the saved model;
#' @param version, which is an integer representing the version number of the model.
# If a model with the same name and version already exists in the "models" directory,
# the function will print a warning and increment the version number before saving the model.

save_model <- function(model_fit, version = 1) {
  # Check if the "models" directory exists
  if (!dir.exists("models")) {
    # If not, create it
    dir.create("models")
  }
  name <- deparse(substitute(model_fit))
  # Construct the file path for the model
  file_path <- paste0("models/", name, "_v", version, ".rds")
  # Check if the file already exists
  if (file.exists(file_path)) {
    # If it does, print a warning and increment the version number
    warning(paste0("Model ", file_path, " already exists! Saving as version ", version + 1, "..."))
    save_model(model_fit, version = version + 1)
  } else {
    # Save the model to the "models" directory with the ".rds" extension
    saveRDS(model_fit, file = file_path)
  }
}

#' Fit a list of models to data and save them to a directory
#' @param data A data frame containing the data to use for model fitting
#' @param model_spec A string containing the path to a JSON file with the model specification
#' @return A list of fitted model objects
fit_models <- function(data, model_spec) {
  # Read the model specification from the JSON file
  models <- jsonlite::fromJSON(model_spec)
  # Fit the models to the data
  fit_list <- lapply(models, function(model) {
    cat(paste0("Fitting ", model$name, " model...\n"))
    fit <- data %>% model(!!model$name := !!model$formula)
    cat(paste0(model$name, " model fitted!\n\n"))
    # Save the model to the "models" directory
    save_model(fit)
    return(fit)
  })
  names(fit_list) <- sapply(models, function(model)
    model$name)
  return(fit_list)
}


##### DATA #####


train <- read_csv("./data/processed/enriched_train.csv")
test <- read_csv("./data/test.csv")

train <-
  mutate(train, ym = yearmonth(date)) %>%
  as_tsibble(key = c(cfips, state, census_region), index = ym) %>%
  fill_gaps() %>%
  select(-row_id)

counties <- distinct(train, cfips, county, state, census_region)
ground_truth <- select(train, cfips, activity)

train_hts <- train %>%
  aggregate_key((cfips * state) * census_region, active = sum(active)) %>%
  filter(!is_aggregated(census_region))

plan(multisession)
#' break up training data to observe accuracy
train_sample <- filter(train, date <= "2022-07-01")
test_sample <- filter(train, date > "2022-07-01") %>% select(cfips, activity)

#' Model fitting
tslm_fit <- train_sample %>%  model(tslm = TSLM(activity ~ trend() + season(period = 12)))
ets_fit <- train_sample %>%  model(ets = ETS(activity ~ trend() + season(period = 12, method = "A") + error(method = "A")))
theta_fit <- train_sample %>% model(theta = THETA(activity ~ season(period  = 12)))
arima_fit <- train_sample %>% model(arima = ARIMA(activity))

#' Forecasts
tslm_forecasts <- get_forecasts(tslm_fit, h = 8)
ets_forecasts <- get_forecasts(ets_fit, h = 8)
theta_forecasts <- get_forecasts(theta_fit, h = 8)
arima_forecasts <- get_forecasts(arima_fit, h = 8)
#PCA
xreg <- train %>% as_tibble() %>% select(activity:pct_it_workers)
pca <- xreg %>% na.omit() %>% prcomp(xreg)

#' Fable combination forecasting
fit <- train_sample %>%
  model(
    snaive = SNAIVE(activity),
    tslm = TSLM(activity ~ trend() + season(period = 12)),
    ets = ETS(activity ~ trend() + season(period = 12, method = "A") + error(method = "A")),
    ar  = AR(activity),
    rw = ARIMA(activity ~ pdq(d = 1))
  )
#' combine models
fit <- mutate(fit, comb = (snaive + tslm + ets + ar + rw)/5)
#' generate probabilistic forecast for combination model 
fit_futures <- fit %>%
  # Generate 1000 future sample paths
  generate(h = "2 years", times = 1000) %>%
  # Compute forecast distributions from future sample paths
  as_tibble() %>%
  group_by(ym, .model) %>%
  summarise(
    dist = distributional::dist_sample(list(.sim))
  ) %>%
  ungroup() %>%
  # Create fable object
  as_fable(index = ym, key = .model,
           distribution = dist, response = "activity")

ets_model <- combine_model_fits(ets_fit, ets_forecasts)
tslm_model <- combine_model_fits(tslm_fit, tslm_forecasts)
theta_model <- combine_model_fits(theta_fit, theta_forecasts)
arima_model <- combine_model_fits(arima_fit, arima_forecasts)

arima_train_fit <-
  arima_fit %>%
  fitted() %>%
  as_tibble() %>%
  select(-.model) %>%
  left_join(ground_truth) %>%
  mutate(fit = "Training")

# arima_train_fit <- 
#   arima_fit %>%
#   fitted() %>%
#   as_tibble() %>%
#   left_join(ground_truth)

ets_model <- combine_model_fits(ets_fit, ets_forecasts)
tslm_model <- combine_model_fits(tslm_fit, tslm_forecasts)
theta_model <- combine_model_fits(theta_fit, theta_forecasts)
arima_model <- combine_model_fits(arima_fit, arima_forecasts)

#' save training models
save_model(ets_fit)
save_model(tslm_fit)
save_model(theta_fit)