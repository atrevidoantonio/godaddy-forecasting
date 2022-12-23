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

df <- readr::read_csv("./data/train.csv") %>%
  transmute(row_id,
            cfips,
            county,
            state,
            date = ymd(first_day_of_month),
            activity = microbusiness_density,
            active)

glimpse(df)

bea_key <- "78B3F189-18EA-4FA7-9F24-06FE9640FA87"

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
              year = TimePeriod,
              code = Code,
              value = DataValue)
  print("Data retrieved!")
  return(dataframe)
}

beaGet()
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
           year = TimePeriod,
           code = Code,
           value = DataValue)
  print("Data retrieved!")
  return(dataframe)
}

vars <- c("400", "500", "700", "800", "900","1000","1100", "1200", "1800")

beaCaemp <- NULL
for (var in vars) {
  print(paste("Retreiving data for linecode:", var))
  beaCaemp <- rbind(beaCaemp, bea_caemp(var, bea_key))
}

#' #' total population
#' bea_pops <- bea_cainc(linecode = 100, bea_key)
#' #' per-capita income
#' bea_income <- bea_cainc(linecode = 110, bea_key)
#' #' total employment
#' bea_cainc_empl <- bea_cainc(linecode = 240, bea_key)
#' #' wage and salary employees 
#' bea_workers <- bea_cainc(linecode = 250, bea_key)
#' #' total proprietors
#' bea_props <- bea_cainc(linecode = 260, bea_key)
#' 
#' beaCainc <-  rows_append(bea_pops, bea_income) %>%
#'   rows_append(bea_cainc_empl) %>%
#'   rows_append(bea_workers) %>%
#'   rows_append(bea_props)

cainc_vars <- c("100", "110", "240", "250", "260")
beaCainc <- NULL
for (var in cainc_vars) {
  print(paste("Retreiving data for linecode:", var))
  beaCainc <- rbind(beaCainc, bea_cainc(var, bea_key))
}
