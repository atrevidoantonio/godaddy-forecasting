#!/usr/bin/Rscript
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
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
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)
library(feasts)
library(seasonal)
library(zoo)
library(tigris)
library(sf)
sf::sf_use_s2(use_s2 = FALSE)
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
      text = element_text(family = "Franklin Gothic Book", color = charcoal),
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
  group_split(cfips)
county_names <- distinct(train, county, state) %>%
  transmute(county = paste(county, state, sep = " "))
county_names <- unique(county_names$county)
#' name the elements of the list
names(counties) <- county_names
#sort(unique(counties$cfips)) -> county_names
#' perform tests at the county level
dickey_fuller_tests <- lapply(counties, unit_root_test) %>% plyr::ldply(tibble)
kpss_tests <- lapply(counties, unit_root_test, test = "KPSS") %>% plyr::ldply(tibble)
pp_tests <- lapply(counties, unit_root_test, test = "PP") %>% plyr::ldply(tibble)

#' generate state total microbusiness firms and relative shares
train <- train %>%
  group_by(state, year) %>%
  mutate(state_firms = sum(active)) %>%
  ungroup() %>%
  mutate(share = active/state_firms)
#' standardize and normalize activity
train <- train %>%
  group_by(date) %>%
  mutate(
    density_z = (activity - mean(activity)) / sd(activity),
    density_scaled = (activity - min(activity)) / (min(activity) + max(activity))
  ) %>%
  ungroup()

cbsa_density <-
  read_csv("./data/processed/cbsa_density.csv") %>% group_by(date) %>%
  mutate(
    density_z = (activity - mean(activity)) / sd(activity),
    density_scaled = (activity - min(activity)) / (min(activity) + max(activity))
  ) %>%
  ungroup()

###### Spatial data #####
fips_codes <- tigris::fips_codes %>%
  mutate(cfips = paste(state_code, county_code, sep = "") %>% sub("^0+", "", .) %>% as.numeric(.)) %>%
  transmute(cfips,
            sfips = state_code,
            state = state_name)

cbsa_sf <- tigris::core_based_statistical_areas() %>% janitor::clean_names()

cbsa_sf <- transmute(cbsa_sf,
                     cbsa_code = cbsafp,
                     cbsa = name,
                     lat = as.numeric(intptlat),
                     lon = as.numeric(intptlon))

counties_sf <- tigris::counties() %>% janitor::clean_names() %>%
  transmute(cfips = geoid,
            sfips = statefp,
            county = name,
            aland,
            awater,
            lat = as.numeric(intptlat),
            lon = as.numeric(intptlon)) %>%
  mutate(cfips = sub("^0+", "", cfips) %>% as.numeric(.)) %>%
  left_join(fips_codes)

states_sf <- tigris::states() %>% filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

alaska <- tigris::states() %>% filter(STATEFP == "02") %>% erase_water()
hawaii <- tigris::states() %>% filter(STATEFP == "15") %>% erase_water()

con_cbsa_sf <- cbsa_sf %>%
  filter(
    !cbsa_code %in% c(
      "10380",
      "11640",
      "11260",
      "17620",
      "17640",
      "21820",
      "25020",
      "25900",
      "27580",
      "27940",
      "27980",
      "28180",
      "28540",
      "32420",
      "38660",
      "41980",
      "41900",
      "42180",
      "46520",
      "49500"
    )
  ) %>%
  mutate(cbsa_code = as.numeric(cbsa_code))
#' join to contingous CBSAs

cbsa_density <- left_join(cbsa_density, con_cbsa_sf) %>% arrange(cbsa_code, date) %>%
  mutate(year = year(date))
#' join to counties 
county_sf <- left_join(train, counties_sf)

#' add water features to map
#' focus on Pacific areas for a closer look
pacific_sf <- filter(con_cbsa_sf, year == 2021, census_region == "Pacific")
#' convert to list
pacific_sf <- group_split(pacific_sf, cbsa_code)
#' erase water
pacific_sf <- lapply(pacific_sf, erase_water)
pacific_sf <- plyr::ldply(pacific_sf, as_tibble)

#' Same as above but at county level
pacific_counties <- filter(counties_sf, state %in% c("California", "Oregon", "Washington"))
pacific_counties <- group_split(pacific_counties, cfips)
pacific_counties <- lapply(pacific_counties, erase_water)
pacific_counties <- plyr::ldply(pacific_counties, as_tibble)

#' create a county level data frame for contiguous Pacific states 
pacific <- filter(train, state %in% c("California", "Oregon", "Washington")) %>%
  left_join(pacific_counties)

gc()

#### Plots ######

plot_kde <- function(data, x, log_scale = FALSE, group = TRUE, percent = FALSE) {
  # Set the x-axis label based on the x variable
  x_labels <- c(activity = "Mircobusiness activity (per 100)",
                active = "Active Microbusinesses",
                share = "Mircobusiness activity (share of state total)")
  x_label <- x_labels[x_var]
  p <- ggplot(data, aes(x = !!sym(x_var))) + theme_clean() + labs(x = x_label, y = "Density\n")
  if (group) {
    p <- ggplot(data, aes(x = x, linetype = as.factor(year))) +
      stat_density(geom = "line",
                   position = "identity",
                   color = "black") +
      theme_clean() +
      guides(color = "none") +
      labs(x = x_label, y = "Density\n", linetype = "")
  }
  # Format the x axis as a percentage if specified
  if (percent) {
    p <- p + scale_x_continuous(guide = "axis_minor", labels = percent_format())
  } else {
    p <- p + scale_x_continuous(guide = "axis_minor", labels = comma_format())
  }
  # Add a log scale for the x axis if specified
  if (log_scale) {
    p <- p + scale_x_log10(guide = "axis_minor", labels = comma_format())
  }
  # Return the plot
  return(p)
}
#' kernel density estimation

plot_kde(train, x = train$active, group = TRUE, log_scale = TRUE)
plot_kde(train, train$activity, log_scale = TRUE)
plot_kde(train, train$share, percent = TRUE)

#' How does this look across regions
ggplot(train, aes(x = active, linetype = as.factor(year), color = as.factor(year))) +
  stat_density(geom = "line", position = "identity") +
  scale_x_log10(guide = "axis_minor", labels = comma_format()) +
  scale_y_continuous(guide = "axis_minor") +
  theme_clean() +
  guides(color = "none") +
  scale_color_manual(values = c(onyx, rhythm, gainsboro, metallic)) +
  # guides(color = "none", linetype = guide_legend(
  # override.aes = list(color = c(sapphire, rainy_day, persian_orange, eton)))) +
  # scale_color_manual(values = c(sapphire, rainy_day, persian_orange, eton)) +
  labs(x = "\nMircobusiness activity (count active firms)", y = "Density\n", linetype = "") +
  facet_wrap(~census_region)

#' How does this look across regions
ggplot(train, aes(x = activity, linetype = as.factor(year), color = as.factor(year))) +
  stat_density(geom = "line", position = "identity") +
  scale_x_log10(guide = "axis_minor", labels = comma_format()) +
  scale_y_continuous(guide = "axis_minor") +
  theme_clean() +
  guides(color = "none") +
  # guides(color = "none", linetype = guide_legend(
  # override.aes = list(color = c(sapphire, rainy_day, persian_orange, eton)))) +
  scale_color_manual(values = c(onyx, rhythm, gainsboro, metallic)) +
#  scale_color_manual(values = c(sapphire, rainy_day, persian_orange, eton)) +
  labs(x = "\nMircobusiness activity (count active firms)", y = "Density\n", linetype = "", color = "") +
  facet_wrap(~census_region)

#' How does this look across regions
ggplot(train, aes(x = activity, linetype = as.factor(year), color = as.factor(year))) +
  stat_density(geom = "line", position = "identity") +
  scale_x_log10(guide = "axis_minor", labels = comma_format()) +
  scale_y_continuous(guide = "axis_minor") +
  theme_clean() +
  guides(color = "none") +
  scale_color_manual(values = c(onyx, rhythm, gainsboro, metallic)) +
  labs(x = "\nMircobusiness activity (count active firms)", y = "Density\n", linetype = "") +
  facet_wrap(~census_region)

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
    color = satin,
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
  labs(x = "\nIncome per-capita", y = "Microbusiness Density\n", size = "Population") +
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

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      college = mean(pct_college, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  college, y = activity, size = population)
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
  labs(x = "College educated population", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      for_born = mean(pct_foreign_born, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  for_born, y = activity, size = population)
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
  labs(x = "Immigrant population", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")


ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      pct_bb = mean(pct_bb, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  pct_bb, y = activity, size = population)
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
  labs(x = "Population with Internet access", y = "Microbusiness Density", size = "Population") +
  facet_wrap( ~ census_region, scales = "free")

ggplot(
  train %>%
    group_by(cfips, census_region, year) %>%
    summarize(
      median_hh_inc = mean(median_hh_inc, na.rm = TRUE),
      population = max(population),
      activity = mean(activity)
    ) %>%
    ungroup(),
  aes(x =  median_hh_inc, y = activity, size = population)
) +
  geom_jitter(color = sapphire) +
  stat_smooth(
    method = "lm",
    linewidth = 0.75,
    color = raspberry,
    se = FALSE
  ) +
  scale_x_continuous(labels = dollar_format(), guide = "axis_minor") +
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
  labs(x = "Median household income", y = "Microbusiness Density", size = "Population") +
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

ggplot(con_cbsa_sf %>% filter(year == 2021)) +
  geom_sf(data = states_sf, fill = gainsboro) +
  geom_sf(aes(fill = log(activity))) +
  scale_fill_gradient(low = prussian,  high = "#3C153B") +
  theme_void()


# ggplot(con_cbsa_sf %>% filter(year == 2021, census_region == "Pacific")) +
#   geom_sf(data = states_sf %>% filter(NAME %in% c("Washington", "Oregon", "California")), fill = "gray90") +
#   geom_sf(aes(fill = activity)) +
#   scale_fill_distiller(palette = "PuBu") +
#   theme_void()

ggplot(pacific_sf %>% filter(year == 2021), aes(geometry = geometry)) +
  geom_sf(data = states_sf %>% filter(NAME %in% c("Washington", "Oregon", "California")), fill = "gray90") +
  geom_sf(aes(fill = activity)) +
  scale_fill_distiller(palette = "PuBu") +
  theme_void()

ggplot(pacific %>% filter(year == 2021), aes(geometry = geometry)) +
  geom_sf(aes(fill = activity)) +
  scale_fill_distiller(palette = "PuBu") +
  theme_void()
