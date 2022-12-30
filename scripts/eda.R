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

train <- read_csv("./data/enriched_train.csv")

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

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      unem = mean(unem, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  unem / 100, y = activity, size = population)
) +
  geom_jitter(color = sapphire) +
  stat_smooth(
    method = "lm",
    linewidth = 0.75,
    color = raspberry,
    se = FALSE
  ) +
  scale_x_continuous(labels = percent_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(
    override.aes = list(color = sapphire, linetype = NA),
    nrow = 2
  )) +
  theme_clean() +
  labs(x = "Unemployment rate", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")

ggplot(
  train %>%
    group_by(cfips, year) %>%
    summarize(
      income = mean(income_per_capita, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ),
  aes(x =  income, y = activity, size = population)
) +
  geom_jitter(color = sapphire) +
  theme_clean() +
  scale_x_log10(labels = dollar_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(
    override.aes = list(color = sapphire, linetype = NA),
    nrow = 2
  )) +
  labs(x = "Income per-capita", y = "Microbusiness Density")

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      income = mean(income_per_capita, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  income, y = activity, size = population)
) +
  scale_x_log10(labels = dollar_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  geom_jitter(color = sapphire) +
  stat_smooth(
    method = "lm",
    linewidth = 0.75,
    color = raspberry,
    se = FALSE
  ) +
  theme_clean() +
  guides(size = guide_legend(
    override.aes = list(color = sapphire, linetype = NA),
    nrow = 2
  )) +
  labs(x = "Income per-capita", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      manufacturing = mean(manufacturing / total_employment, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  manufacturing, y = activity, size = population)
) +
  geom_jitter(color = sapphire) +
  stat_smooth(
    method = "lm",
    linewidth = 0.75,
    color = raspberry,
    se = FALSE
  ) +
  scale_x_continuous(labels = percent_format(), guide = "axis_minor") +
  scale_y_continuous(guide = "axis_minor") +
  scale_size_continuous(
    range = c(1, 5),
    labels = comma_format(),
    breaks = c(1e5, 1e6, 25e5, 5e6)
  ) +
  guides(size = guide_legend(
    override.aes = list(color = sapphire, linetype = NA),
    nrow = 2
  )) +
  theme_clean() +
  labs(x = "Manufacturing", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")

ggplot(train %>%
         filter(
           state %in% c(
             "Washington",
             "Oregon",
             "California",
             "Idaho",
             "Texas",
             "New York",
             "North Carolina"
           )
         ),
       aes(x =  date, y = activity, group = cfips)) +
  geom_line(color = sapphire) +
  facet_wrap(~ census_region, scales = "free") +
  theme_clean()
