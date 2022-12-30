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

##### Helper Functions #####

unit_root_test <- function(tbl, test = "ADF") {
  #' custom function to implement several flavors of unit root tests for stationary time series
  #' defaults to the Augmented Dickey-Fuller test,but supports Kwiatkowski–Phillips–Schmidt–Shin (KPSS)
  #' and Phillips-Peron tests. Please note that for the KPSS test 
  #' the presence of a unit root is not the null hypothesis but the alternative.
  if (test == "ADF") {
    result = adf.test(tbl$congested_sites)
    result = tidy(result)
    return(result)
  }
  else if (test == "KPSS") {
    result = unitroot_kpss(tbl$congested_sites)
    result = tidy(result) %>% 
      pivot_wider(names_from = names, values_from = x) %>% 
      rename(statistic = kpss_stat, p.value = kpss_pvalue) %>% 
      mutate(method = "Kwiatkowski–Phillips–Schmidt–Shin Test", 
             alternative = "not stationary")
    return(result)
  }
  else if (test == "PP") {
    result = unitroot_pp(tbl$congested_sites)
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
      text = element_text(color = onyx),
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
      text = element_text(family = "Franklin Gothic Book", color = charcoal),
      plot.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(colour = 'black')
    )
}

sapphire <- "#255F85"
rainy_day <- "#474C5c"
raspberry <- "#DB2955"
onyx <- "#383C42"
slate <- "#3f3f3f"
charcoal <- "#2E4057"
dark_magenta <- "#861B54"
dark_emerald <- "#2d6d66"
##### DATA #####
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
df <- readr::read_csv("./data/train.csv") %>%
  transmute(row_id,
            cfips,
            county = gsub(" County", "", county),
            state,
            date = ymd(first_day_of_month),
            year = year(first_day_of_month),
            activity = microbusiness_density,
            active) %>%
  left_join(states)

glimpse(df)
#' set API keys
bea_key <- "78B3F189-18EA-4FA7-9F24-06FE9640FA87"

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
#' bea_fips <- distinct(bea_table, cfips)
#' train_fips <- distinct(df, cfips)            
#' inner_join(lau_fips, train_fips)
bea_table <- left_join(cainc_wide, caemp_wide) %>% mutate(cfips = as.numeric(cfips), year = as.numeric(year))

train <- left_join(df, bea_table, by = c("cfips", "county", "year")) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(lau_wide)

ts <-  mutate(train, ym = yearmonth(date)) %>%
  as_tsibble(key = cfips, index = ym) %>%
  fill_gaps()

#' ACF plots
ts %>% ACF(activity) %>% autoplot() +
  labs(x = "", y = "ACF\n") +
  facet_wrap(~census_region, scales = "free") + theme_clean()

#' PACF plots
ts %>% PACF(activity) %>% autoplot() + 
  labs(x = "", y = "PACF") +
  facet_wrap(~census_region, scales = "free") + th

#' convert to list for easy implementation of unit root tests
counties <-
  train %>% 
  group_split(cfips, county)
#' name the elements of the list
sort(unique(counties$cfips)) -> county_names
names(counties) <- county_names
#' perform tests at the county level
dickey_fuller_tests <- lapply(counties, unit_root_test) %>% plyr::ldply(tibble)
kpss_tests <- lapply(counties, unit_root_test, test = "KPSS") %>% plyr::ldply(tibble)
pp_tests <- lapply(counties, unit_root_test, test = "PP") %>% plyr::ldply(tibble)

ggplot(train %>%
         group_by(cfips, census_region, year) %>%
         summarize(unem = mean(unem, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)) %>%
         ungroup(), 
       aes(x =  unem/100, y = activity, size = population)) +
  geom_jitter(color = sapphire) +
  stat_smooth(method = "lm", linewidth = 0.75, color = raspberry, se = FALSE) +
  scale_x_continuous(labels = percent_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(override.aes = list(color = sapphire, linetype = NA), nrow = 2)) +
  theme_clean() +
  labs(x = "Unemployment rate", y = "Microbusiness Density", size = "Population") +
  facet_wrap(~census_region, scales = "free")

ggplot(train %>%
         group_by(cfips, year) %>%
         summarize(income = mean(income_per_capita, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)), aes(x =  income, y = activity, size = population)) + 
  geom_jitter(color = sapphire) +
  theme_clean() +
  scale_x_log10(labels = dollar_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(override.aes = list(color = sapphire, linetype = NA), nrow = 2)) +
  labs(x = "Income per-capita", y = "Microbusiness Density")

ggplot(train %>%
         group_by(cfips, census_region, year) %>%
         summarize(income = mean(income_per_capita, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)) %>%
         ungroup(), 
       aes(x =  income, y = activity, size = population)) + 
  scale_x_log10(labels = dollar_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  geom_jitter(color = sapphire) +
  stat_smooth(method = "lm", linewidth = 0.75, color = raspberry, se = FALSE) +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = sapphire, linetype = NA), nrow = 2)) +
  labs(x = "Income per-capita", y = "Microbusiness Density", size = "Population") +
  facet_wrap(~census_region, scales = "free")

ggplot(train %>%
         group_by(cfips, census_region, year) %>%
         summarize(manufacturing = mean(manufacturing/total_employment, na.rm = TRUE),
                   population = max(population),
                   activity = mean(activity)) %>%
         ungroup(), 
       aes(x =  manufacturing, y = activity, size = population)) +
  geom_jitter(color = sapphire) +
  stat_smooth(method = "lm", linewidth = 0.75, color = raspberry, se = FALSE) +
  scale_x_continuous(labels = percent_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(override.aes = list(color = sapphire, linetype = NA), nrow = 2)) +
  theme_clean() +
  labs(x = "Manufacturing", y = "Microbusiness Density", size = "Population") +
  facet_wrap(~census_region, scales = "free")

ggplot(
  train %>%
    filter(state %in% c("Washington", "Oregon", "California", "Idaho", "Texas", "New York", "North Carolina")),
  aes(x =  date, y = activity, group = cfips)
) +
  geom_line(color = sapphire) +
  facet_wrap( ~ census_region, scales = "free") +
  theme_clean()
