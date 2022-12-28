#!/usr/bin/Rscript
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggsci)
library(scales)
library(tibbletime)
library(distributional)
library(ggdist)
library(lubridate)
library(ggh4x)
library(hrbrthemes)
library(fredr)
library(bea.R)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)
library(feasts)
library(seasonal)
library(tigris)
library(sf)
#' load in training data
df <- readr::read_csv("./data/train.csv") %>%
  transmute(row_id,
            cfips,
            county = gsub(" County", "", county),
            state,
            date = ymd(first_day_of_month),
            activity = microbusiness_density,
            active)

glimpse(df)
#' set API keys
bea_key <- "78B3F189-18EA-4FA7-9F24-06FE9640FA87"
fred_key 
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
#' Provide a list of linecodes (variables) for data retrieval
caemp_vars <- c("400", "500", "700", "800", "900", "1000", "1100", "1200", "1800")
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
  filter(period != "13")  %>%
  mutate(date = ym(date))
#' Pivot the lau dataframe to a wide format and clean up county fips codes for merging
lau_wide <- pivot_wider(lau, names_from = series) %>%
  mutate(cfips = sub("^0+", "", cfips) %>% as.numeric) %>%
  select(cfips, date, year, unem = `Unemployment rate`)

#' Sanity check
#' lau_fips <- distinct(lau, cfips) %>% mutate(cfips = sub("^0+", "", cfips) %>% as.numeric)
#' train_fips <- distinct(df, cfips)            
#' inner_join(lau_fips, train_fips)
bea_table <- left_join(cainc_wide, caemp_wide) %>% mutate(cfips = as.numeric(cfips))

train <- left_join(df, bea_table) %>% 
  mutate(year = as.numeric(year)) %>%
  left_join(lau_wide)


ggplot(train %>%
         group_by(cfips, year) %>%
         summarize(unem = mean(unem, na.rm = TRUE),
                   population = mean(population),
                   activity = mean(activity)), aes(x =  unem, y = activity, size = population)) + 
  geom_jitter()

ggplot(train %>%
         group_by(cfips, year) %>%
         summarize(income = mean(income_per_capita, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)), aes(x =  income, y = activity, size = population)) + 
  geom_jitter()


ggplot(train %>%
         group_by(cfips, year) %>%
         summarize(manufacturing = mean(manufacturing, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)), aes(x =  manufacturing, y = activity, size = population)) + 
  geom_jitter()

ggplot(
  train %>%
    filter(
      cfips == 53033 |
        cfips == 53053 |
        cfips == 53061 |
        cfips == 41051 |
        cfips == 41059 |
        cfips == 29095
    ),
  aes(x =  date, y = activity)
) +
  geom_line(color = sapphire) +
  facet_wrap( ~ county, scales = "free") +
  theme_clean()
