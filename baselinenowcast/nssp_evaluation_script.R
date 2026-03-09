library(baselinenowcast)
library(lubridate)
library(dplyr)
library(scoringutils)

eval_window <- 25 
scale_factor <- 3
prop_delay <- 0.5
max_delay <- 25

long_df <- syn_nssp_df |>
  mutate(delay = as.integer(report_date - reference_date))
# Set up the nowcast dates that you want to produce retrospective nowcasts for
nowcast_dates <- seq(from = ymd("2025-12-10"), 
                     to = ymd("2026-04-01"), by = "week")
mult_nowcasts <- c()
for(i in seq_along(nowcast_dates)){
  # Data used for nowcasting
  training_data <- long_df |> 
    filter(report_date <= nowcast_dates[i],
           reference_date <= nowcast_dates[i]) 
  # initial reports used for comparison
  training_df_by_ref_date <- training_data |>
    group_by(reference_date) |>
    summarise(initial_count = sum(count))
  
  # Generate the nowcast
  rep_tri_full <- as_reporting_triangle(training_data)
  rep_tri <- truncate_to_delay(rep_tri_full, max_delay = max_delay)
  # Note, could also add another loop to iterate through different training
  # volumes and history for delay estimation
  nowcast_draws <- baselinenowcast(rep_tri,
                                   scale_factor = scale_factor,
                                   prop_delay = prop_delay,
                                   draws = 1000)
  # Could evaluate the draws directly using CRPS, or summarise into quantiles
  # and evaluate the quantiles. Will show both options
  nowcast_summary_df <-
    nowcast_draws |>
    group_by(reference_date) |>
    summarise(
      `q_.50` = median(pred_count),
      `q_.25` = quantile(pred_count, 0.25),
      `q_.75` = quantile(pred_count, 0.75),
      `q_.05` = quantile(pred_count, 0.05),
      `q_.95` = quantile(pred_count, 0.95),
      `q_.025` = quantile(pred_count, 0.025),
      `q_.975` = quantile(pred_count, 0.975)
    ) |>
    # Add metadata for this iteration of the loop
    mutate(
      nowcast_date = nowcast_dates[i],
      scale_factor = scale_factor,
      prop_delay = prop_delay
    )
  
  eval_data <- long_df |>
    filter( #delay <= max_delay,
           reference_date <= nowcast_dates[i]) |>
    group_by(reference_date) |>
    summarise(final_count = sum(count))
  
  nowcast_w_data <- nowcast_summary_df |>
    left_join(training_df_by_ref_date,
              by = "reference_date") |>
    left_join(eval_data,
              by = "reference_date") 
  # Combine the nowcasts from each iteration 
  mult_nowcasts <- bind_rows(mult_nowcasts, nowcast_w_data)
}

# Scoring nowcasts using scoringutils-----------------------------------

# Include only the days for which we want to evaluate. All `max_delay` days
# are being nowcasted, but we might only want to evaluate say the last X days.
mult_nowcasts_for_eval <- mult_nowcasts |>
  filter(reference_date >= nowcast_date - days(eval_window))

# Pivot from wide to long
mult_nowcasts_long <- mult_nowcasts_for_eval |>
  pivot_longer(
    cols = starts_with("q_"),
    names_prefix = "q_",
    names_to = "quantile_level",
    values_to = "pred_count") |>
  mutate(quantile_level = as.numeric(quantile_level))

su_quantiles <- scoringutils::as_forecast_quantile(
  mult_nowcasts_long,
  forecast_unit = c("nowcast_date",
                    "reference_date",
                    "scale_factor",
                    "prop_delay"),
  observed = "final_count",
  predicted = "pred_count",
  quantile_level = "quantile_level")

# This will return a WIS score, broken down into overprediction, 
# underprediction, and dispersion + bias and interval coverage (50th and 90th)
# for each reference date and nowcast date.
scores <- scoringutils::score(su_quantiles)

# Can plot scores over time by nowcast date, and, if you have different 
# training volumes (e.g. scale_factor and/or prop_delay also vary, can 
# compare scores across those permutations)

scores_by_nowcast_date <- scores |>
  summarise_scores(by = "nowcast_date")

ggplot(scores_by_nowcast_date) +
  geom_line(aes(x = nowcast_date, y = wis)) +
  theme_bw() +
  xlab("Nowcast date") +
  ylab("Average WIS")


# Visual comparison of nowcasts------------------------------------------
# Make a plot to visually compare a few nowcasts to the initial and 
# final count data

nowcast_dates_to_visualize <-  seq(from = ymd("2025-12-10"), 
                                   to = ymd("2026-04-01"), by = "4 weeks")

mult_nowcasts_filtered <- mult_nowcasts_for_eval |>
  filter(nowcast_date %in% nowcast_dates_to_visualize) 

ggplot(mult_nowcasts_filtered) + 
  geom_line(aes(x = reference_date, y = final_count, 
                group = nowcast_date,
                linetype = "Final evaluation data"),
                color = "red") +
  geom_line(aes(x = reference_date, y = initial_count,
                group = nowcast_date,
                linetype = "Data as of nowcast date"), 
            color = "pink") +
  geom_vline(aes(xintercept = nowcast_date,
                 linetype = "Date of nowcast")) +
  geom_line(aes(x = reference_date, y = `q_.50`, group = nowcast_date),
            color = "gray")+
  geom_ribbon(aes(x = reference_date, ymin = `q_.025`, ymax = `q_.975`, 
                  group = nowcast_date), fill = "gray", alpha = 0.25) +
  geom_ribbon(aes(x = reference_date, ymin = `q_.25`, ymax = `q_.75`,
                  group = nowcast_date), fill = "gray", alpha = 0.25) +
  theme_bw() +
  scale_linetype_manual(
    name = "Observed data",
    values = c(
      "Final evaluation data" = "solid",
      "Data as of nowcast date" = "solid",
      "Date of nowcast" = "dashed"
    ),
    breaks = c(
      "Final evaluation data",
      "Data as of nowcast date",
      "Date of nowcast"
    ),
    guide = guide_legend(
      override.aes = list(
        color = c(
          "Final evaluation data" = "red",
          "Data as of nowcast date" = "pink",
          "Date of nowcast" = "black"
        ),
        linewidth = 1
      )
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  xlab("Reference date") +
  ylab("Incident BAR cases") 
