tar_load(clean_daily_data)
tar_load(clean_weekly_data)

bar_weekly_data <- clean_weekly_data |>
  filter(pathogen == "bar")

#Multipliers 
multipliers_bar <- clean_daily_data |>
  filter(pathogen == "bar") |>
  group_by(reference_date, report_date) |>
  summarise(count = sum(count)) |>
  mutate(delay = as.integer(report_date - reference_date)) |> 
  group_by(reference_date) |>
  arrange(report_date) |>
  mutate(cumreceived = cumsum(count),
         totalreceived = max(cumreceived),
         percentreceived = cumreceived/totalreceived,
         endofweek = pmax(1, ceiling(delay/7)) # Delay days 0 through 8 get counted in the 1 week multiplier
  ) |>
  arrange(reference_date) |>
  group_by(reference_date, endofweek) |>
  filter(percentreceived == max(percentreceived))|>
  group_by(endofweek) |>
  summarize("2.5%" =quantile(percentreceived,probs=0.025),
            median =quantile(percentreceived,probs=0.5),
            "97.5%" =quantile(percentreceived,probs=0.975))

# My attempt at correcting so that the first 7 days from the reference date are coded as week 1
multipliers_bar_revised <- clean_daily_data |>
  filter(pathogen == "bar") |>
  group_by(reference_date, report_date) |>
  summarise(count = sum(count)) |>
  mutate(delay = as.integer(report_date - reference_date)) |> 
  group_by(reference_date) |>
  arrange(report_date) |>
  mutate(cumreceived = cumsum(count),
         totalreceived = max(cumreceived),
         percentreceived = cumreceived/totalreceived,
         endofweek = floor(delay/7) + 1  # Delay days 0 through 7 get counted in the 1 week multiplier
  ) |>
  arrange(reference_date) |>
  group_by(reference_date, endofweek) |>
  filter(percentreceived == max(percentreceived))|>
  group_by(endofweek) |>
  summarize("2.5%" =quantile(percentreceived,probs=0.025),
            median =quantile(percentreceived,probs=0.5),
            "97.5%" =quantile(percentreceived,probs=0.975))

# Multipliers from weekly 
multipliers_bar_rev2 <- clean_weekly_data |>
  rename(reference_date = end_of_week_reference_date) |>
  group_by(reference_date) |>
  mutate(cumreceived = cumsum(count),
         totalreceived = max(cumreceived),
         percentreceived = cumreceived/totalreceived,
         endofweek = delay + 1  # Delay days 0 through 7 get counted in the 1 week multiplier
  ) |>
  arrange(reference_date) |>
  group_by(reference_date, endofweek) |>
  filter(percentreceived == max(percentreceived))|>
  group_by(endofweek) |>
  summarize("2.5%" =quantile(percentreceived,probs=0.025),
            median =quantile(percentreceived,probs=0.5),
            "97.5%" =quantile(percentreceived,probs=0.975))


# Produce nowcast

nowcast_date <- "2024-10-16"

bar_data_to_nowcast <- bar_weekly_data |>
  rename(reference_date = end_of_week_reference_date,
         report_date = end_of_week_report_date) |>
  filter(report_date <= nowcast_date,
         reference_date <= nowcast_date,
         reference_date >= ymd(nowcast_date) - weeks(9)
  ) |>
group_by(reference_date, pathogen) |>
summarise(n_reported = sum(count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    nowcast_date = nowcast_date,
    weeks_ago = as.integer((ymd(nowcast_date) - reference_date) / 7L) + 1L,
  ) |>
  arrange(reference_date)

final_counts <- bar_weekly_data |>
  rename(reference_date = end_of_week_reference_date) |>
  group_by(reference_date, pathogen) |>
  summarise(final_count = sum(count, na.rm = TRUE), .groups = "drop") |>
  arrange(reference_date)

coefficients<-multipliers_bar_rev2|> 
  mutate(weeks_ago = as.integer(as.numeric(endofweek))) |>
  filter(weeks_ago <= 9)


nowcast_data <- bar_data_to_nowcast |>
  left_join(final_counts, by = c("pathogen", "reference_date")) |>
  left_join(coefficients, by = "weeks_ago") |>
  mutate(med_val = n_reported/median, 
         ub = n_reported/`2.5%`,
         lb = n_reported/`97.5%`)

ggplot(nowcast_data) + 
  geom_line(aes(x = reference_date, y = med_val)) +
  geom_ribbon(aes(x = reference_date, ymin = lb, ymax = ub), alpha = 0.3) +
  geom_point(aes(x = reference_date, y = final_count)) + theme_bw() 

