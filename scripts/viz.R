
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
      text = element_text(color = onyx, family = "Arial Narrow"),
      strip.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      axis.ticks.length = unit(0.5, "cm"),
      axis.ticks = element_line(linewidth = 0.15)
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
      text = element_text(family = "Roboto Condensed", color = charcoal),
      plot.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(colour = 'black')
    )
}

classic_blue <- "#0F4C81"
sapphire <- "#255F85"
sky_blue <- "#6596CD"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
pink <- "#EFAAC4"
raspberry <- "#C33149"
violet <- "#5f4b8b"
dark_violet <- "#351C45"
onyx <- "#383C42"
dim_gray <- "#6B717E"
gainsboro <- "#D8D8D8"
metallic <- "#837A75"
slate <- "#3f3f3f"
charcoal <- "#2E4057"
claret <- "#8B1E3F"
pepper <- "#9b1b30"
dark_magenta <- "#861B54"
eton <- "#739F8F"
emerald <- "#00997b"
dark_emerald <- "#2d6d66"

train_hts %>%
  filter(!is_aggregated(cfips), !is_aggregated(state)) %>%
  filter(census_region == "Pacific") %>%
  autoplot() +
  labs(x = "", y = "Active firms") +
  scale_y_continuous(guide = "axis_minor", labels = comma_format()) +
  guides(color = "none") +
  facet_wrap(~state, scales = "free_y") +
  theme_clean()

train_hts %>%
  filter(is_aggregated(cfips), !is_aggregated(state)) %>%
  autoplot() +
  labs(x = "", y = "Active firms") +
  scale_y_continuous(guide = "axis_minor", labels = comma_format()) +
  guides(color = "none") +
  facet_wrap(~state, scales = "free_y") +
  theme_clean()

train_hts %>%
  filter(is_aggregated(cfips), !is_aggregated(state), !is_aggregated(census_region)) %>%
  autoplot() +
  labs(x = "", y = "Active firms") +
  scale_y_continuous(guide = "axis_minor", labels = comma_format()) +
  guides(color = "none") +
  facet_wrap(~census_region, scales = "free_y") +
  theme_clean()

train_hts %>%
  filter(is_aggregated(cfips), is_aggregated(state)) %>%
  autoplot() +
  labs(x = "", y = "Active firms") +
  scale_y_continuous(guide = "axis_minor", labels = comma_format()) +
  guides(color = "none") +
  scale_color_aaas() +
  facet_wrap(~census_region, scales = "free_y") +
  theme_clean()

train %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_line(aes(y = activity)) + 
  facet_wrap(vars(cfips), scales = "free") +
  theme_clean()

ets_model %>%
  left_join(counties) %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")

ets_model %>%
  left_join(counties) %>%
  filter(state == "Oregon") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")

tslm_model %>%
  left_join(counties) %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = ym)) +
  geom_ribbon(aes(ymin = `95_upper`, ymax = `95_lower`, fill = "Forecast"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Actual")) + 
  theme_clean() +
  scale_color_manual(values = classic_blue) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(vars(county), scales = "free")

#ggsave("washington_small_multiples.svg", plot = last_plot(), dpi = 720, height = 8, width = 12)

#' refitting model
ets_refit <- refit(ets_fit, train) %>%
  get_forecasts(h = 8)

fct <- ets_refit %>%
  select(ym:census_region, activity = .mean, lower = `95_lower`, upper = `95_upper`) %>%
  select(-.model) %>%
  mutate(model = "Forecast") %>%
  bind_rows(ground_truth) %>%
  arrange(cfips, ym) %>%
  mutate(model = if_else(is.na(model), "Current", as.character(model)),
         date = lubridate::ym(ym) %>% floor_date(., unit = "month")) %>%
  left_join(counties)

fct %>%
  filter(state == "Washington") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = upper, ymax = lower, fill = "95% CI"), alpha = 0.25) + 
  geom_line(aes(y = activity, color = "Current")) +
  #' hacky work around since use color = model, splits the lines
  geom_line(data = fct %>% filter(state == "Washington", model == "Forecast"), aes(y = activity, color = "Forecast")) +
  theme_clean() +
  scale_color_manual(values = c(sky_blue, rainy_day)) +
  scale_fill_manual(values = pepper) +
  labs(x = "", y = "Activity", color = "", fill = "") +
  facet_wrap(~county, scales = "free")
