# This script is used to produce examples of forecasts from the flu metrocast hub

# It will focus on few specific dates and models and showing both visual 
# comparison of forecasts alongside scores

url_prefix <- ("https://raw.githubusercontent.com/reichlab/flu-metrocast/refs/heads/main/model-output/")
truth_data_url <- ("https://raw.githubusercontent.com/reichlab/flu-metrocast/refs/heads/main/target-data/latest-data.csv")

model_names <- c("epiENGAGE-baseline", "UT-GBQR","epiforecasts-dyngam", 
                 "VTSanghani-PRIME", "NAU-Copycat")
reference_dates <- c("2025-11-22", "2025-11-29", "2025-12-06", "2025-12-13", "2025-12-20", "2026-01-03",
                     "2026-01-24", "2026-01-31", "2026-02-07", "2026-02-14")
locations <- c("nyc", "boston", "indianapolis")

# Load in all the data for all of the examples
combo_df <- expand.grid(model_name = model_names, 
                        reference_date = reference_dates)
df_all <- c()
for(i in 1:nrow(combo_df)) {
  url <- file.path(url_prefix,
            glue::glue("{combo_df$model_name[i]}/{combo_df$reference_date[i]}-{combo_df$model_name[i]}.csv")
  )
  df_i <- tryCatch(read_csv(url) |>
    mutate(model = as.character(combo_df$model_name[i])),
    error = function(e) NULL)
  df_all <- bind_rows(df_all, df_i)
}

truth_data <- read_csv(truth_data_url) |> 
  filter(location %in% locations)

df <- df_all |>
  filter(location %in% locations) |>
  left_join( truth_data, by = c("target_end_date", "location", "target"))

# Score and compute coverage 
scores <- df |>
  as_forecast_quantile(
    predicted = "value",
    observed = "observation",
    quantile_level = "output_type_id",
    forecast_unit = c(
      "location",
      "reference_date",
      "model",
      "target_end_date"
    )
  ) |>
  transform_forecasts(fun = log_shift, offset = 1) |>
  score() 

scores_log <- scores |>
  filter(scale == "log")
scores_natural <- scores |>
  filter(scale == "natural")

df_for_plotting <- df |>
  filter(
  output_type_id %in% c(0.025, 0.25, 0.5, 0.75, 0.975))|>
  pivot_wider(id_cols = c("reference_date", "target_end_date", "location", "model", "observation"),
              names_from = output_type_id, 
              values_from = value,
              names_prefix = "q_")
# Plotting stuff-------------------------------------------------------------

get_plot_theme <- function(dates = TRUE) {
  plot_theme <- cowplot::theme_half_open() +
    cowplot::background_grid() +
    theme(
      plot.background = element_rect(fill = "white"),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 20),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 16),
      strip.text = element_text(size = 16),
      strip.background = element_rect(fill = "white")
    )
  if (isTRUE(dates)) {
    plot_theme <- plot_theme +
      theme(
        axis.text.x = element_text(
          vjust = 1,
          hjust = 1,
          angle = 45,
          size = 11
        )
      )
  }
  
  return(plot_theme)
}

plot_components <- function() {
  pal_models <- brewer.pal(4, "Spectral")
  model_colors <- c(
    "epiENGAGE-baseline" = "gray",
    "epiforecasts-dyngam" = pal_models[1],
    "NAU-Copycat" = pal_models[2],
    "UT-GBQR" = pal_models[3],
    "VTSanghani-PRIME" = pal_models[4]
  )
  plot_comp_list <-
    list(
      model_colors = model_colors
    )
  return(plot_comp_list)
}

plot_comps <- plot_components()

        

# Early growth in NYC (Nov 22, Dec 6th, Dec 20th)---------------------------
 df_nyc <- df_for_plotting |> filter(
   target_end_date >= "2025-11-01",
   target_end_date <= "2026-01-31",
   !reference_date %in% c("2025-12-20", "2026-01-03","2026-01-24", "2026-02-14",
                          "2026-01-31", "2026-02-17"), 
   location == "nyc",
   model %in% c("epiENGAGE-baseline", 
                "epiforecasts-dyngam", 
                "UT-GBQR")) |> select(- observation) 
     
truth_nyc <- truth_data |> filter(location == "nyc",
               target_end_date >= "2025-11-01",
               target_end_date <= "2026-03-10")
 ggplot(df_nyc) + 
   geom_line(data = truth_nyc, aes(x = target_end_date, y = observation), color = "black") +
   geom_point(data = truth_nyc, aes(x = target_end_date, y = observation), color = "black") +
   geom_line(aes(x = target_end_date, y = `q_0.5`, color = model,
                 group = reference_date)) +
   geom_ribbon(aes(x = target_end_date, ymin = `q_0.25`, ymax = `q_0.75`,
                   fill = model,
                   group = reference_date), alpha = 0.3) +
   geom_ribbon(aes(x = target_end_date, ymin = `q_0.025`, ymax = `q_0.975`,
                   group = reference_date,
                   fill = model), alpha = 0.3) +
   geom_vline(aes(xintercept = reference_date), linetype = "dashed") + 
   xlab('') +
   facet_wrap(~model, nrow = 3) + 
   ylab('ED visits (%)') +
   get_plot_theme(dates = TRUE) +
   scale_color_manual(
     name = "Model",
     values = plot_comps$model_colors
   )+ 
   scale_fill_manual(
     name = "Model",
     values = plot_comps$model_colors
   )+ 
   scale_x_date(
     breaks = "2 weeks",
     date_labels = "%d %b %Y"
   ) +
   ggtitle("New York City: early increase")
 
scores_nyc_log <- scores_log |> filter(
  target_end_date >= "2025-11-01",
  target_end_date <= "2026-01-31",
  !reference_date %in% c("2026-01-03","2026-01-24", "2026-02-14",
                         "2026-01-31"), 
  location == "nyc",
  model %in% c("epiENGAGE-baseline", 
               "epiforecasts-dyngam", 
               "UT-GBQR")) |>
  summarise_scores(by = c("model", "reference_date")) 
p <- ggplot(scores_nyc_log) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  get_plot_theme() +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  xlab("Forecast date") + ylab("Weighted Interval Score (WIS)")+
  ggtitle("Scores after log transformation")

scores_nyc_natural <- scores_natural |> filter(
  target_end_date >= "2025-10-01",
  target_end_date <= "2026-01-31",
  !reference_date %in% c("2026-01-03","2026-01-24", "2026-02-14",
                         "2026-01-31"), 
  location == "nyc",
  model %in% c("epiENGAGE-baseline", 
               "epiforecasts-dyngam", 
               "UT-GBQR")) |>
  summarise_scores(by = c("model", "reference_date")) 
p <- ggplot(scores_nyc_natural) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  theme_bw() +
  get_plot_theme() +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  ggtitle("Natural scale scores")

# Boston at the peak--------------------------------------------------------

df_boston <- df_for_plotting |> filter(
  target_end_date >= "2025-10-01",
  target_end_date <= "2026-01-31",
  reference_date == "2026-01-03", 
  location == "boston") |> 
  select(- observation) 

truth_boston <- truth_data |> filter(location == "boston",
                                  target_end_date >= "2025-10-01",
                                  target_end_date <= "2026-03-10")
ggplot(df_boston) + 
  geom_line(data = truth_boston, aes(x = target_end_date, y = observation), color = "black") +
  geom_point(data = truth_boston, aes(x = target_end_date, y = observation), color = "black") +
  geom_line(aes(x = target_end_date, y = `q_0.5`, color = model,
                group = reference_date)) +
  geom_ribbon(aes(x = target_end_date, ymin = `q_0.25`, ymax = `q_0.75`,
                  fill = model,
                  group = reference_date), alpha = 0.3) +
  geom_ribbon(aes(x = target_end_date, ymin = `q_0.025`, ymax = `q_0.975`,
                  group = reference_date,
                  fill = model), alpha = 0.3) +
  geom_vline(aes(xintercept = reference_date), linetype = "dashed") + 
  xlab('') +
  get_plot_theme(dates = TRUE) +
  scale_color_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  scale_x_date(
    breaks = "2 weeks",
    date_labels = "%d %b %Y"
  ) +
  facet_wrap(~model, nrow = 5) + 
  ylab('ED visits (%)') +
  coord_cartesian(ylim = c(0, 12)) +
  ggtitle("Boston: post-peak decline")

scores_boston_log <- scores_log |> filter(
  target_end_date >= "2025-10-01",
  target_end_date <= "2026-01-31",
  reference_date == "2026-01-03", 
  location == "boston") |>
  summarise_scores(by = c("model", "reference_date")) 
p <- ggplot(scores_boston_log) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  get_plot_theme() +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  xlab("Forecast date") +
  ylab("Weighted Interval Score (WIS)")+
  ggtitle("Scores after log transformation")

scores_boston_natural <- scores_natural |> filter(
  target_end_date >= "2025-10-01",
  target_end_date <= "2026-01-31",
  reference_date == "2026-01-03",
  location == "boston") |>
  summarise_scores(by = c("model", "reference_date")) 
p <- ggplot(scores_boston_natural) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  get_plot_theme() +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  xlab("Forecast date") +
  ylab("Weighted Interval Score (WIS)")+
  ggtitle("Natural scale scores")

# Indianapolis second peak--------------------------------------------

df_ind <- df_for_plotting |> filter(
  target_end_date >= "2025-11-01",
  # target_end_date <= "2026-02-31",
  reference_date %in% c("2026-01-24"), 
  location == "indianapolis")|> select(- observation) 

truth_ind <- truth_data |> filter(location == "indianapolis",
                                  target_end_date >= "2025-11-01",
                                  target_end_date <= "2026-04-10")
ggplot(df_ind) + 
  geom_line(data = truth_ind, aes(x = target_end_date, y = observation), color = "black") +
  geom_point(data = truth_ind, aes(x = target_end_date, y = observation), color = "black") +
  geom_line(aes(x = target_end_date, y = `q_0.5`, color = model,
                group = reference_date)) +
  geom_ribbon(aes(x = target_end_date, ymin = `q_0.25`, ymax = `q_0.75`,
                  fill = model,
                  group = reference_date), alpha = 0.3) +
  geom_ribbon(aes(x = target_end_date, ymin = `q_0.025`, ymax = `q_0.975`,
                  group = reference_date,
                  fill = model), alpha = 0.3) +
  geom_vline(aes(xintercept = reference_date), linetype = "dashed") + 
  xlab('') +
  get_plot_theme(dates = TRUE) +
  scale_color_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  scale_x_date(
    breaks = "2 weeks",
    date_labels = "%d %b %Y"
  ) +
  facet_wrap(~model, nrow = 5) + 
  ylab('ED visits (%)') +
  ggtitle("Indianapolis: second small peak")

scores_ind_log <- scores_log |> filter(
  target_end_date >= "2025-11-01",
  reference_date %in% c("2026-01-24"), 
  location == "indianapolis") |>
  summarise_scores(by = c("model", "reference_date")) 

scores_ind_log_long <- scores_ind_log |>
  pivot_longer(cols = c("overprediction", "underprediction", "dispersion")) |>
  mutate(
    `WIS breakdown` = factor(name, levels = c(
      "overprediction",
      "dispersion",
      "underprediction"
    ))
  )
p <- ggplot(scores_ind_log_long) +
  geom_bar(
    aes(
      x = model, y = value, fill = model,
      alpha = `WIS breakdown`
    ),
    stat = "identity",
    position = "stack"
  ) +
  get_plot_theme() +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  ) +

  guides(
    alpha = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 3,
    ),
    fill = "none"
  ) +
  xlab("") +
  ylab("WIS") +
  theme(axis.text.x = element_blank())
p <- ggplot(scores_ind_log) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  get_plot_theme(dates = TRUE) +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  xlab("Weighted Interval Score (WIS)") +
  ylab("Forecast Date") + 
  ggtitle("Scores after log transformation")

scores_ind_natural <- scores_natural |> filter(
  target_end_date >= "2025-10-01",
  reference_date %in% c("2026-01-24"), 
  location == "indianapolis") |>
  summarise_scores(by = c("model", "reference_date")) 
p <- ggplot(scores_ind_natural) +
  geom_bar(
    aes(
      x = reference_date, y = wis, fill = model
    ),
    stat = "identity",
    position = "dodge"
  ) +
  get_plot_theme(dates = TRUE) +
  scale_fill_manual(
    name = "Model",
    values = plot_comps$model_colors
  )+ 
  xlab("Weighted Interval Score (WIS)") +
  ylab("Forecast Date") + 
  ggtitle("Natural scale scores")





