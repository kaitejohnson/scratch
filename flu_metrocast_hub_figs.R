# This script is used to produce examples of forecasts from the flu metrocast hub

# It will focus on few specific dates and models and showing both visual 
# comparison of forecasts alongside scores


url_prefix <- ("https://raw.githubusercontent.com/reichlab/flu-metrocast/refs/heads/main/model-output/")
truth_data_url <- ("https://raw.githubusercontent.com/reichlab/flu-metrocast/refs/heads/main/target-data/latest-data.csv")

model_names <- c("epiENGAGE-baseline", "UT-GBQR","epiforecasts-dyngam", 
                 "VTSanghani-PRIME")
reference_dates <- c("2025-11-22", "2025-12-06", "2025-12-20", "2026-01-03",
                     "2026-01-24", "2026-02-14")
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

df_for_plotting <- df |>
  filter(
  output_type_id %in% c(0.025, 0.25, 0.5, 0.75, 0.975))|>
  pivot_wider(id_cols = c("reference_date", "target_end_date", "location", "model", "observation"),
              names_from = output_type_id, 
              values_from = value,
              names_prefix = "q_")


        

# Early growth in NYC (Nov 22, Dec 6th, Dec 20th) 
 df_nyc <- df_for_plotting |> filter(
   target_end_date >= "2025-10-01",
   target_end_date <= "2026-01-31",
   !reference_date %in% c("2026-01-03","2026-01-24", "2026-02-14"), 
   location == "nyc",
   model %in% c("epiENGAGE-baseline", 
                "epiforecasts-dyngam", 
                "UT-GBQR")) |> select(- observation) 
     
truth_nyc <- truth_data |> filter(location == "nyc",
               target_end_date >= "2025-10-01",
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
   facet_wrap(~model) + 
   ylab('ED visits (%)') +
   theme_bw() +
   ggtitle("New York City: early increase")



