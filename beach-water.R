library(lubridate)

beaches |>
  filter(!is.na(eColi)) |>
  mutate(year = year(as.Date(collectionDate))) |>
  summarise(median_ecoli = median(eColi), .by = year) |>
  ggplot(aes(x = year, y = median_ecoli)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Median E. coli (CFU/100 mL)")

beaches |>
  filter(!is.na(eColi)) |>
  filter(!is.na(eColi), eColi < 100000) |>
  mutate(month = floor_date(as.Date(collectionDate), "month")) |>
  summarise(mean_ecoli = mean(eColi), .by = month) |>
  ggplot(aes(x = month, y = mean_ecoli)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Month", y = "Mean E. coli (CFU/100 mL)")

clean_beaches <- beaches |>
  filter(!is.na(eColi), eColi < 100000) |>
  mutate(date = as.Date(collectionDate))

# 1) Average by year
clean_beaches |>
  mutate(year = lubridate::year(date)) |>
  summarise(mean_ecoli = mean(eColi), .by = year) |>
  ggplot(aes(x = year, y = mean_ecoli)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Year", y = "Mean E. coli (CFU/100 mL)")

# 2) Average by week of year
clean_beaches |>
  mutate(week = isoweek(date)) |>
  summarise(mean_ecoli = mean(eColi), .by = week) |>
  ggplot(aes(x = week, y = mean_ecoli)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Week of year", y = "Mean E. coli (CFU/100 mL)")

clean_beaches |>
  mutate(year = lubridate::year(date)) |>
  summarise(mean_ecoli = mean(eColi), .by = c(year, beachName)) |>
  ggplot(aes(x = year, y = mean_ecoli)) +
  geom_line() +
  facet_wrap(vars(beachName)) +
  theme_minimal()