#!/usr/bin/Rscript
library(dplyr)
library(tidyverse)
library(bea.R)

#' set API keys
bea_key <- "78B3F189-18EA-4FA7-9F24-06FE9640FA87"

input_data <- "./data/"
processed_data <- "./data/processed/"

load_data <- function(dataframe, input_data) {
  if (input_data) {
    # Load the data from the input data path
    data <- readr::read_csv(paste0(input_data, dataframe, ".csv"))
  } else {
    # Load the data from the processed data path
    data <-
      readr::read_csv(paste0(processed_data, dataframe, ".csv"))
  }
  if (is.null(data)) {
    stop(paste("Error: Could not find file ", file_path))
  }
  return(data)
}

#' save data frame objects to CSV
to_csv <- function(df, path = "./data/processed/") {
  name <- deparse(substitute(df))
  readr::write_csv(df, paste0(path, name, ".csv")) 
}

#' function to work with retrieving data from the BEA for its CAINC table
bea_cainc <- function(linecode = "10", bea_key = bea_key){
  # pass the payload to the BEA API
  payload <- list(
    #' Pass the API key
    "UserID" = bea_key,
    #' Retrieval method
    "Method" = "GetData",
    #' Specify dataset
    "datasetname" = "Regional",
    #' Table within dataset
    "TableName" = "CAINC30",
    #' Specify the particular variable (linecode)
    "LineCode" = linecode,
    #' Specify the geographical level
    "GeoFips" = "County",
    #' Specify years
    "Year" = "2019, 2020, 2021"
  )
  print("Passing payload to API...")
  #' Pass the payload to the API server to retrieve the data
  dataframe <- beaGet(payload, asWide = FALSE) %>%
    #' Initial cleanup of county FIPS and selection
    transmute(cfips = sub("^0+", "", GeoFips),
              county = gsub(",.*$", "", GeoName),
              year = TimePeriod,
              code = Code,
              value = DataValue)
  print("Data retrieved!")
  return(dataframe)
}
#' function to work with retrieving data from the BEA for its CAEMP table
bea_caemp <- function(linecode = "10", bea_key = bea_key){
  payload <- list(
    #' BEA API key
    "UserID" = bea_key,
    #' API method
    "Method" = "GetData",
    #' Dataset
    "datasetname" = "Regional",
    # Specify table within the dataset
    "TableName" = "CAEMP25N",
    #' Specify the particular variable (linecode)
    "LineCode" = linecode,
    #' Specify the geographical level
    "GeoFips" = "County",
    #' Time period
    "Year" = "2019, 2020, 2021"
  )
  print("Passing payload to API...")
  #' Pass the payload to the API server to retrieve the data
  dataframe <- beaGet(payload, asWide = FALSE) %>%
    #' Initial cleanup of county FIPS and selection
    transmute(cfips = sub("^0+", "", GeoFips),
              county = gsub(",.*$", "", GeoName),
              year = TimePeriod,
              code = Code,
              value = DataValue)
  print("Data retrieved!")
  return(dataframe)
}
#' Function to retrieve GDP data from the BEA
bea_gdp <- function(linecode = "1", bea_key = bea_key){
  # pass the payload to the BEA API
  payload <- list(
    #' Pass the API key
    "UserID" = bea_key,
    #' Retrieval method
    "Method" = "GetData",
    #' Specify dataset
    "datasetname" = "Regional",
    #' Table within dataset
    "TableName" = "CAGDP9",
    #' Specify the particular variable (linecode)
    "LineCode" = linecode,
    #' Specify the geographical level
    "GeoFips" = "County",
    #' Specify years
    "Year" = "2019, 2020, 2021"
  )
  print("Passing payload to API...")
  #' Pass the payload to the API server to retrieve the data
  dataframe <- beaGet(payload, asWide = FALSE) %>%
    #' Initial cleanup of county FIPS and selection
    transmute(cfips = sub("^0+", "", GeoFips),
              year = TimePeriod,
              code = Code,
              value = DataValue)
  print("Data retrieved!")
  return(dataframe)
}

st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))

#' Census Regions https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
states <- read.table(
  textConnection(
    "state, census_region
  Alaska, Pacific
  Alabama, East South Central
  Arkansas, West South Central
  Arizona, Mountain
  California, Pacific
  Colorado, Mountain
  Connecticut, New England
  District of Columbia, South Atlantic
  Delaware, South Atlantic
  Florida, South Atlantic
  Georgia, South Atlantic
  Hawaii, Pacific
  Iowa, West North Central
  Idaho, Mountain
  Illinois, East North Central
  Indiana, East North Central
  Kansas, West North Central
  Kentucky, East South Central
  Louisiana, West South Central
  Massachusetts, New England
  Maryland, South Atlantic
  Maine, New England
  Michigan, East North Central
  Minnesota, West North Central
  Missouri, West North Central
  Mississippi, East South Central
  Montana, Mountain
  North Carolina, South Atlantic
  North Dakota, West North Central
  Nebraska, West North Central
  New Hampshire, New England
  New Jersey, Middle Atlantic
  New Mexico, Mountain
  Nevada, Mountain
  New York, Middle Atlantic
  Ohio, East North Central
  Oklahoma, West South Central
  Oregon, Pacific
  Pennsylvania, Middle Atlantic
  Rhode Island, New England
  South Carolina, South Atlantic
  South Dakota, West North Central
  Tennessee, East South Central
  Texas, West South Central
  Utah, Mountain
  Virginia, South Atlantic
  Vermont, New England
  Washington, Pacific
  Wisconsin, East North Central
  West Virginia, South Atlantic
  Wyoming, Mountain"), 
  sep = ',',
  colClasses = c('character', 'character'),
  header = TRUE
) %>%
  mutate(state = trimws(state),
         census_region = trimws(census_region))

#' load in training data
train <- readr::read_csv("./data/train.csv") %>%
  transmute(row_id,
            cfips,
            county = gsub(" County", "", county),
            state,
            date = ymd(first_day_of_month),
            year = year(first_day_of_month),
            activity = microbusiness_density,
            active) %>%
  left_join(states)

#' Provide a list of linecodes (variables) for data retrieval
caemp_vars <- c("90", "400", "500", "700", "800", "900", "1000", "1100", "1200", "1800")
beaCaemp <- NULL
for (var in caemp_vars) {
  print(paste("Retreiving data for linecode:", var))
  beaCaemp <- rbind(beaCaemp, bea_caemp(var, bea_key))
}

#' Provide a list of linecodes (variables) for data retrieval
cainc_vars <- c("100", "110", "240", "250", "260")
bea_Cainc <- NULL
for (var in cainc_vars) {
  print(paste("Retreiving data for linecode:", var))
  bea_Cainc <- rbind(bea_Cainc, bea_cainc(var, bea_key))
}

payload <- list(
  #' Pass the API key
  "UserID" = bea_key,
  #' Retrieval method
  "Method" = "GetData",
  #' Specify dataset
  "datasetname" = "Regional",
  #' Table within dataset
  "TableName" = "CAGDP9",
  #' Specify the particular variable (linecode)
  "LineCode" = "1",
  #' Specify the geographical level
  "GeoFips" = "County",
  #' Specify years
  "Year" = "2019, 2020, 2021"
)

cGDP <- beaGet(payload, asWide = FALSE) |>
  transmute(cfips = sub("^0+", "", GeoFips),
            county = gsub(",.*$", "", GeoName),
            year = TimePeriod,
            gdp = DataValue)

unique(beaCaemp$code)

#' Set up linecodes
caemp_linecodes <- c(
  "CAEMP25N-90",
  "CAEMP25N-400",
  "CAEMP25N-500",
  "CAEMP25N-700",
  "CAEMP25N-800",
  "CAEMP25N-900",
  "CAEMP25N-1000",
  "CAEMP25N-1100",
  "CAEMP25N-1200",
  "CAEMP25N-1800"
)
#' Get the associated names for the linecode 
caemp_names <- c(
  "Private Nonfarm",
  "Construction",
  "Manufacturing",
  "Retail trade",
  "Transport/warehousing",
  "Information",
  "Finance",
  "Insurance",
  "Professional",
  "Accommodation"
)
#' Put CAEMP linecode and name together
caemp_table <- tibble(code = caemp_linecodes, varname = caemp_names)

cainc_linecodes <- c(
  "CAINC30-100", #Population
  "CAINC30-110", #Income per-capita
  "CAINC30-240", #Total employment
  "CAINC30-250", #Wage and salary
  "CAINC30-260" #Total proprietors
)

cainc_names <- c(
  "Population",
  "Income per-capita",
  "Total employment",
  "Workers",
  "Proprietors"
)

cainc_table <- tibble(code = cainc_linecodes, varname = cainc_names)

#' Using the variable names above, combine with the BEA data and pivot wide
cainc_wide <- left_join(bea_Cainc, cainc_table) %>%
  select(cfips, county, year, varname, value) %>%
  pivot_wider(names_from = varname, 
              values_from = value) %>%
  janitor::clean_names()

caemp_wide <- left_join(beaCaemp, caemp_table) %>%
  select(cfips, county, year, varname, value) %>%
  pivot_wider(names_from = varname, 
              values_from = value) %>%
  janitor::clean_names()
#' Download and store BLS LAU statistics into a dataframe
lau <- read_tsv(url("https://download.bls.gov/pub/time.series/la/la.data.64.County"))
#' Clean up the data, removing some regular expressions, getting a date, and proper names
lau <-
  #' Remove data prefix
  mutate(lau, label = gsub("LAUCN", "", series_id)) %>% 
  #' Trim to 15 characters to max BLS series identifiers, 
  #' trim to first five characters for county FIPS
  #' and convert period to month
  mutate(label = strtrim(label, 15),
         cfips = strtrim(label, 5),
         period = gsub("M", "", period),
         value = as.numeric(value),
         #' Detach last character for variable code
         series_code = stringi::stri_sub(series_id, -1, -1)) %>%
  #' For each series, provide its common use name
  mutate(date = paste(year, period, sep = "-"), 
         series = case_when(series_code == 3 ~ "Unemployment rate",
                            series_code == 4 ~ "Unemployment (level)",
                            series_code == 5 ~ "Employment",
                            series_code == 6 ~ "Labor force")) %>%
  select(cfips, date, year, month = period, series, value) %>%
  #' Remove aggregate
  filter(month != "13")  %>%
  mutate(date = ym(date))
#' Pivot the LAU data frame to a wide format and clean up county FIPS codes for merging
lau_wide <- pivot_wider(lau, names_from = series) %>%
  mutate(cfips = sub("^0+", "", cfips) %>% as.numeric) %>%
  select(cfips, date, year, unem = `Unemployment rate`, uneml = `Unemployment (level)`, empl = `Employment`, labforce = `Labor force`)

bea_table <- left_join(cainc_wide, caemp_wide) %>% mutate(cfips = as.numeric(cfips), year = as.numeric(year))

census_df <- read_csv("./data/census_starter.csv") %>% relocate(cfips) %>%
  #' change from wide to long
  gather(name, percent, -cfips) %>%
  #' remove underscores to extract variable name and year
  mutate(year = str_extract(name, "(?<=_)[^_]*$") %>% as.numeric(.),
         name = sub("_[^_]+$", "", name)) %>%
  #' change back to wide
  spread(name, percent) %>%
  #' convert percent values to actual decimal
  mutate(across(starts_with("pct_"), ~ .x/100))

#' join data together
enriched_train <- left_join(train, bea_table, by = c("cfips", "county", "year")) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(lau_wide) %>%
  left_join(census_df)

#' load in CBSA areas from Census Bureau
cbsa_codes <-
  read_csv("./data/cbsa_codes.csv") %>% janitor::clean_names() %>%
  mutate(cfips = paste(fips_state_code, fips_county_code, sep = "")) %>%
  mutate(cfips = sub("^0+", "", cfips) %>% as.numeric) %>%
  transmute(cfips,
            cbsa_code,
            county = gsub(" County", "", county_county_equivalent),
            cbsa = cbsa_title,
            state = state_name,
            cbsa_type = metropolitan_micropolitan_statistical_area) %>%
  arrange(cfips)

counties <- distinct(train, cfips)
cbsas <- inner_join(cbsa_codes, counties)

#' enrich the training data frame wtih CBSAs
enriched_train <- left_join(enriched_train, cbsas) %>%
  relocate(census_region, .after = state) %>%
  relocate(cbsa_code, .after = cfips) %>%
  relocate(cbsa, .after = county) %>%
  relocate(cbsa_type, .after = census_region) %>%
  mutate(cbsa_type = if_else(is.na(cbsa_type), "Unattached County", as.character(cbsa_type)),
         cbsa = if_else(is.na(cbsa), "Unattached", as.character(cbsa)))
#' fill in missing data
enriched_train <-
  enriched_train %>% group_by(cfips) %>% fill(c(
    population,
    median_hh_inc,
    pct_bb,
    pct_college,
    pct_foreign_born,
    pct_it_workers
  ),
  .direction = "down")

#' might be better to retrieve the CBSA population for Micro and Metro areas
#' from the BEA API or Census API
cbsa_pop <- read_csv("./data/cbsa_pop.csv") %>%
  mutate(cbsa = stringr::str_replace(cbsa, "\\*", "") %>% trimws(.)) %>%
  pivot_longer(cols = 3:5,
               values_to = "population",
               names_to = "year") %>%
  mutate(year = stringr::str_replace(year, "population_", "") %>% as.numeric(.))

cbsa_density <-
  train %>%
  group_by(cbsa_code, cbsa, date, year) %>%
  summarize(active = sum(active)) %>%
  left_join(cbsa_pop) %>%
  group_by(cbsa_code, cbsa) %>%
  fill(population) %>%
  mutate(abb = sub("^[^,]*,", '', cbsa) %>% sub("(^[^-]+)-.*", "\\1", .) %>% trimws(.)) %>%
  filter(!is.na(cbsa_code)) %>%
  left_join(st_crosswalk) %>%
  select(-abb) %>%
  left_join(states) %>%
  mutate(activity = active/population*100) %>%
  arrange(cbsa_code, date)

#' Sanity check
#lau_fips <- distinct(lau, cfips) %>% mutate(cfips = sub("^0+", "", cfips) %>% as.numeric) %>% mutate(lau = "LAU")
#bea_fips <- distinct(bea_table, cfips) %>% mutate(bea = "BEA")
#train_fips <- distinct(df, cfips, county, state)
#' inner_join(lau_fips, train_fips)
#bea_fips <- left_join(bea_fips, lau_fips)
#train_fips <- left_join(train_fips, bea_fips)
#train_fips <- left_join(train_fips, lau_fips)

#' save data
to_csv(enriched_train)
to_csv(lau_wide)
to_csv(bea_table)
to_csv(cbsas)
to_csv(cbsa_density)
to_csv(census_df)