library(dplyr)
library(ggplot2)
library(forcats)

# Activity by hour
raccoons |>
  summarise(mean_observed = mean(units_observed), .by = hour) |>
  ggplot(aes(x = hour, y = mean_observed)) +
  geom_line() +
  theme_minimal()

# Confidence vs standoff duration
raccoons |>
  ggplot(aes(x = raccoon_confidence_level, y = avg_standoff_duration_sec)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_minimal()

# Ward ranking
raccoons |>
  summarise(mean_bins = mean(bins_compromised), .by = ward_name) |>
  ggplot(aes(x = mean_bins, y = fct_reorder(ward_name, mean_bins))) +
  geom_col() +
  theme_minimal()

# Season
raccoons |>
  summarise(total = sum(units_observed), .by = date) |>
  mutate(date = as.Date(date)) |>
  ggplot(aes(x = date, y = total)) +
  geom_line() +
  theme_minimal()

count(raccoons, proximity_to_a_patio)

library(sf)

wards <- search_packages("city wards") |>
  list_package_resources() |>
  filter(name == "City Wards Data - 4326.geojson") |>
  get_resource()

glimpse(wards)

ward_bins <- raccoons |>
  summarise(mean_bins = mean(bins_compromised), .by = ward_id)

brks <- c(4, 5, 6, 6.8)

wards |>
  mutate(ward_id = as.integer(AREA_SHORT_CODE)) |>
  left_join(ward_bins, by = "ward_id") |>
  ggplot() +
  geom_sf(fill = "grey95", colour = "grey60") +
  stat_sf_coordinates(aes(size = mean_bins, colour = mean_bins), alpha = 0.8) +
  scale_size("Mean bins compromised", range = c(2, 18), breaks = brks) +
  scale_colour_viridis_c("Mean bins compromised", breaks = brks, guide = "legend") +
  theme_void()

