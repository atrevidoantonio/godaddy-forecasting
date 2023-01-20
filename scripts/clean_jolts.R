library(tidyverse)
library(stringr)
library(readr)

save_data <- function(tibble, path = "processed") {
  valid_paths <- c("processed", "output", "general")
  if (!(path %in% valid_paths)) {
    stop("Invalid path. Must be one of: processed, output, general.")
  }
  name <- deparse(substitute(tibble))
  readr::write_csv(tibble, paste0("./data/", path, "/", name, ".csv"))
}

clean_jolts <- function(tseries, data) {
  jolts <- read_tsv(url(tseries))
  #' clean up and decompose series_id into separate elements
  jolts <-
    jolts %>%
    mutate( #' keep only the first 9 characters of the series_id column
      prefix = substr(series_id, 1, 9),
      #' Trim the first 9 chracters from the "series_id" column
      series = substr(series_id, 10, nchar(series_id))
    ) %>%
    #' Keep the last six characters of the "prefix" column and store it as a new column "industry"
    mutate(industry = substr(prefix, 4, 9)) %>%
    mutate(
      # keep only the first 3 characters of the prefix column
      prefix = substr(prefix, 1, 3),
      #' Keep the first two characters of the "series" column and store it as a new column "state"
      state = substr(series, 1, 2),
      # remove the first 2 characters of the series column and then keep only the first five characters
      area = substr(series, 3, nchar(series_id)) %>% substr(., 1, 5),
      # remove the first 7 characters of the series column and then keep only the first two characters
      size = substr(series, 8, nchar(series_id)) %>% substr(., 1, 2),
      # remove the first 9 characters of the series column and then keep only the first two characters
      variable = substr(series, 10, nchar(series_id)) %>% substr(., 1, 2),
      # keep only the last character in the series column
      ratelevel = substr(series, 12, nchar(series_id))
    ) %>%
    mutate(date = paste(year, str_remove(period, "M"), sep = "-"))

  # sanity test to see if series_id has been properly decomposed
  if (!all(
    jolts$series_id == paste(
      jolts$prefix,
      jolts$industry,
      jolts$state,
      jolts$area,
      jolts$size,
      jolts$variable,
      jolts$ratelevel,
      sep = ""
    )
  )) {
    stop("Decomposition of series_id column failed!")
  }
  #' filter data to aggregates at state level and only level for data not rates
  jolts <- filter(jolts, state != "00" & industry == "000000" & size == "00" & ratelevel == "L") %>%
    filter(!state %in% c("MW", "NE", "SO", "WE"), prefix == "JTS")
  #' keep decomposed elements
  jolts <-
    jolts %>%
    select(date,
      year,
      fips = state,
      value
    ) %>%
    rename(!!data := value)
  return(jolts)
}

urls <-
  c(
    "https://download.bls.gov/pub/time.series/jt/jt.data.3.Hires",
    "https://download.bls.gov/pub/time.series/jt/jt.data.2.JobOpenings",
    "https://download.bls.gov/pub/time.series/jt/jt.data.5.Quits",
    "https://download.bls.gov/pub/time.series/jt/jt.data.4.TotalSeparations",
    "https://download.bls.gov/pub/time.series/jt/jt.data.6.LayoffsDischarges"
  )
#' specify column names and get data
data_cols <- c("hires", "openings", "quits", "separations", "discharges")
jolts_df <- lapply(seq_along(urls), function(i) clean_jolts(urls[i], data = data_cols[i]))
#' merge together
merged_jolts <- jolts_df[[1]]
for (i in 2:length(jolts_df)) {
  merged_jolts <- left_join(merged_jolts, jolts_df[[i]], by = c("date", "year", "state"))
}

#' convert thousands to full number
merged_jolts <- mutate(merged_jolts, across(hires:discharges, ~ .x * 1000))
#' state crosswalk to attach state name to fips codes
state_crosswalk <- maps::state.fips %>%
  transmute(
    fips = str_pad(fips, 2, pad = "0"),
    state = str_extract(str_to_title(polyname), "^[^:]+")
  ) %>%
  distinct(fips, state) %>%
  #' add Alaska and Hawaii
  add_row(fips = "15", state = "Hawaii") %>%
  add_row(fips = "02", state = "Alaska")

merged_jolts <-
  left_join(merged_jolts, state_crosswalk) %>%
  relocate(state, .after = year) %>%
  select(-fips) %>%
  mutate(date = lubridate::ym(date))

save_data(merged_jolts, path = "processed")

rm(merged_jolts, jolts_df, urls, data_cols, state_crosswalk)
gc()
