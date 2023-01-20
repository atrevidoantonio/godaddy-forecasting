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
    fct <- fabletools::forecast(mbl, h = h) %>%
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

save_data <- function(tibble, path = "processed") {
  valid_paths <- c("processed", "output", "general")
  if (!(path %in% valid_paths)) {
    stop("Invalid path. Must be one of: processed, output, general.")
  }
  name <- deparse(substitute(tibble))
  readr::write_csv(tibble, paste0("./data/", path, "/", name, ".csv"))
}


format_fcts <- function(fcts, forecast = FALSE) {
  if (forecast == FALSE) {
    fct_submission <- fcts %>%
      as_tibble() %>%
      transmute(
        row_id = paste(cfips, lubridate::ym(ym), sep = "_"),
        microbusiness_density = .mean
      )
  } else {
    fct_submission <- fcts %>%
      forecast(h = dmonths) %>%
      as_tibble() %>%
      transmute(
        row_id = paste(cfips, lubridate::ym(ym), sep = "_"),
        microbusiness_density = .mean
      )
  }
  return(fct_submission)
}

forecast_predictors <-
  function(data,
           column,
           h = dmonths,
           method = ETS,
           rename_output = TRUE,
           ...) {
    model <- model(fit = method(data[, column]))
    forecast_result <- forecast(model, h = h) %>%
      as_tibble()
    if (rename_output) {
      forecast_result <- forecast_result %>%
        select(ym, cfips, state, census_region, !!as.name(column) := .mean)
    }
    return(forecast_result)
  }

predictors <- select(train_subset, active:cases_pop) %>% colnames()

