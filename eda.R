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

df <- readr::read_csv("./data/train.csv")

mutate(df, ymd(date))

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
  #' Pass the payload to the API server to retrieve the data
  dataframe <- beaGet(payload, asWide = FALSE) %>%
    #' Initial cleanup of county FIPS and selection
    transmute(cfips = sub("^0+", "", GeoFips),
              year = TimePeriod,
              code = Code,
              value = DataValue)
  return(dataframe)
}

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
  #' Pass the payload to the API server to retrieve the data
  dataframe <- beaGet(payload, asWide = FALSE) %>%
    #' Initial cleanup of county FIPS and selection
    transmute(cfips = sub("^0+", "", GeoFips),
           year = TimePeriod,
           code = Code,
           value = DataValue)
  return(dataframe)
}

bea_caemp500 <- bea_caemp(linecode = 500, bea_key)
bea_caemp700 <- bea_caemp(linecode = 700, bea_key)
bea_caemp1800 <- bea_caemp(linecode = 1800, bea_key)
#' total population
bea_pops <- bea_cainc(linecode = 100, bea_key)
#' per-capita income
bea_income <- bea_cainc(linecode = 110, bea_key)
#' total employment
bea_cainc_empl <- bea_cainc(linecode = 240, bea_key)
#' wage and salary employees 
bea_workers <- bea_cainc(linecode = 250, bea_key)
#' total proprietors
bea_props <- bea_cainc(linecode = 260, bea_key)

beaCainc <-  rows_append(bea_pops, bea_income) %>%
  rows_append(bea_cainc_empl) %>%
  rows_append(bea_workers) %>%
  rows_append(bea_props)

beaCaemp <-  rows_append(bea_caemp500, bea_caemp700) %>%
  rows_append(bea_caemp1800) %>%
  rows_append(bea_workers) %>%
  rows_append(bea_props)
