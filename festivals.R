ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU", "SRT", "SHP")) |>
  mutate(hour = as.integer(substr(Time, 1, 2))) |>
  summarise(mean_delay = mean(Min.Delay), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = mean_delay, colour = Line)) +
  geom_line() +
  theme_minimal() +
  scale_colour_viridis_d() +
  labs(x = "Hour of day", y = "Mean delay (minutes)", colour = "Line")

ttc |>
  filter(Min.Delay > 0, Line == "YU", substr(Time, 1, 2) == "04") |>
  arrange(desc(Min.Delay)) |>
  select(Date, Time, Station, Code, Min.Delay) |>
  head(10)

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU", "SHP"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(hour = as.integer(substr(Time, 1, 2))) |>
  summarise(mean_delay = mean(Min.Delay), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = mean_delay, colour = Line)) +
  geom_line() +
  theme_minimal() +
  scale_colour_viridis_d() +
  labs(x = "Hour of day", y = "Mean delay (minutes)", colour = "Line")

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(hour = as.integer(substr(Time, 1, 2))) |>
  summarise(mean_delay = mean(Min.Delay), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = mean_delay, colour = Line)) +
  geom_line() +
  theme_minimal() +
  scale_colour_viridis_d(end = 0.7) +
  labs(x = "Hour of day", y = "Mean delay (minutes)", colour = "Line")

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(hour = as.integer(substr(Time, 1, 2))) |>
  summarise(n = n(), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = n, colour = Line)) +
  geom_line() +
  theme_minimal() +
  scale_colour_viridis_d(end = 0.7) +
  labs(x = "Hour of day", y = "Number of delays", colour = "Line")

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU", "SHP"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(hour = as.integer(substr(Time, 1, 2))) |>
  summarise(n = n(), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = n, fill = Line)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d(end = 0.8) +
  labs(x = "Hour of day", y = "Number of delays", fill = "Line")


ord <- c(6:23, 0:1)
lab <- c("6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
         "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM",
         "8 PM", "9 PM", "10 PM", "11 PM", "12 AM", "1 AM")

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU", "SHP"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(hour = factor(as.integer(substr(Time, 1, 2)),
                       levels = ord, labels = lab)) |>
  summarise(n = n(), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = n, fill = Line)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d(end = 0.8) +
  guides(x = guide_axis(angle = 90)) +
  labs(x = "Hour of day", y = "Number of delays", fill = "Line")

ord <- c(6:23, 0:1)
lab <- c("6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
         "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM",
         "8 PM", "9 PM", "10 PM", "11 PM", "12 AM", "1 AM")

line_names <- c(
  YU = "Line 1 Yonge-University",
  BD = "Line 2 Bloor-Danforth",
  SHP = "Line 4 Sheppard"
)

ttc |>
  filter(Min.Delay > 0, Line %in% c("BD", "YU", "SHP"),
         !substr(Time, 1, 2) %in% c("02", "03", "04", "05")) |>
  mutate(
    hour = factor(as.integer(substr(Time, 1, 2)), levels = ord, labels = lab),
    Line = factor(line_names[Line], levels = line_names)
  ) |>
  summarise(n = n(), .by = c(hour, Line)) |>
  ggplot(aes(x = hour, y = n, fill = Line)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d(end = 0.8) +
  guides(x = guide_axis(angle = 90)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", colour = NA)) +
  labs(x = "Hour of day", y = "Number of delays", fill = "Line")