error_tbl <- mutate(
  error_tbl,
  high_error = if_else(forecast >= 0.25, 1, 0),
  high_pop = if_else(population >= 25000, 1, 0),
  active_threshold = if_else(active >= 5000, 1, 0)
)

error_tbl <- select(error_tbl, -active_threshold)

error_tbl <- mutate(
  error_tbl,
  high_error = if_else(forecast >= 0.25, 1, 0),
  high_pop = if_else(population >= 25000, 1, 0),
  active_threshold = if_else(active >= 5000, 1, 0)
)

error_tbl <- as_tibble(error_tbl)

cutoff_values <- seq(500, 20000, by = 50)
error_results <- data.frame(cutoff = numeric(), avg_forecast = numeric())
for (cutoff in cutoff_values) {
  filtered_data <- filter(error_tbl, active <= cutoff)
  avg_forecast <- summarize(filtered_data, forecast = mean(forecast, na.rm = TRUE))
  error_results <- rbind(error_results, data.frame(cutoff = cutoff, avg_forecast = avg_forecast$forecast))
}

cutoff_values <- seq(500, 20000, by = 50)
results <- data.frame(active_threshold = numeric(), avg_forecast = numeric(), above_threshold = character())
for (cutoff in cutoff_values) {
  #' temporary dataframe for generating results
  filtered_data <-
    mutate(error_tbl, active_threshold = if_else(active <= cutoff, 1, 0)) %>%
    mutate(above_threshold = if_else(active_threshold == 1, "Below", "Above"))
  #' get the average forecast error
  avg_forecast <-
    group_by(filtered_data, active_threshold, above_threshold) %>%
    summarize(forecast = mean(forecast, na.rm = TRUE),
              n = n())
  #' combine results
  results <-
    rbind(
      results,
      data.frame(
        active_threshold = cutoff,
        avg_forecast = avg_forecast$forecast,
        above_threshold = avg_forecast$above_threshold,
        counties = avg_forecast$n
      )
    )
}

ggplot(error_results, aes(x = cutoff, y = avg_forecast/100)) +
  geom_step(color = classic_blue) +
  scale_x_comma(guide = "axis_minor") +
  scale_y_percent(guide = "axis_minor") +
  labs(x = "Threshold for (minimum) active firms", y = "Forecast error (SMAPE)", color = "") +
  theme_clean()

ggplot(results, aes(x = active_threshold, y = avg_forecast/100, color = above_threshold)) +
  geom_step() +
  scale_x_comma(guide = "axis_minor") +
  scale_y_percent(guide = "axis_minor") +
  ggsci::scale_color_d3() +
  labs(x = "Threshold for (minimum) active firms", y = "Forecast error (SMAPE)", color = "") +
  theme_clean()

ggplot(results, aes(x = active_threshold, y = counties/3150, color = above_threshold)) +
  geom_step() +
  scale_x_comma(guide = "axis_minor") +
  scale_y_percent(guide = "axis_minor") +
  ggsci::scale_color_d3() +
  labs(x = "Threshold for (minimum) active firms", y = "Number of counties", color = "") +
  theme_clean()


ggplot(
  error_tbl,
  aes(
    x = training / 100,
    y = forecast / 100,
    size = population,
    color = census_region
  )
) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(1e3, 1e4, 1e5, 1e6)
  ) +
  geom_jitter() +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = pink), nrow = 2),
         color = guide_legend(nrow = 3)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(x = "\nTraining error (SMAPE)", y = "Test error (SMAPE)\n", size = "Population", color = "Region") +
  facet_wrap(~census_region, scales = "free")

ggplot(
  error_tbl,
  aes(
    x = training / 100,
    y = forecast / 100,
    size = population,
    color = census_region
  )
) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(1e3, 1e4, 1e5, 1e6)
  ) +
  geom_jitter() +
  theme_clean() +
  guides(size = guide_legend(override.aes = list(color = pink), nrow = 2),
         color = "none") +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(x = "\nTraining error (SMAPE)", y = "Test error (SMAPE)\n", size = "Population", color = "Region") +
  facet_wrap(~census_region, scales = "free")

ggplot(error_tbl, aes(x = population, y = forecast/100, size = training/100, color = census_region)) +
  scale_size_continuous(
    range = c(0.1, 10),
    labels = percent_format(),
  ) +
  geom_jitter() +
  theme_clean() +
  scale_x_log10(labels = comma_format()) +
  scale_y_continuous(labels = percent_format()) +
  guides(size = guide_legend(override.aes = list(color = pink), nrow = 2),
         color = "none") +
  labs(x = "\nPopulation size", y = "Test error (SMAPE)\n", size = "Training error (SMAPE)", color = "Region") +
  facet_wrap(~census_region, scales = "free")

ggplot(error_tbl, aes(x = active, y = forecast/100, size = population, color = census_region)) +
  scale_size_continuous(
    range = c(0.5, 10),
    labels = comma_format(),
    breaks = c(1e3, 1e4, 1e5, 1e6)
  ) +
  geom_jitter() +
  theme_clean() +
  stat_smooth(color = charcoal, se = FALSE, size = 0.75, method = "gam") +
  guides(size = guide_legend(override.aes = list(color = pink), nrow = 2),
         color = "none") +
  scale_x_log10(labels = comma_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(x = "\nActive firms", y = "Test error (SMAPE)\n", size = "Population", color = "Region") +
  facet_wrap(~census_region, scales = "free")
