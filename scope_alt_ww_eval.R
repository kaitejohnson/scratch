# This is a script to see if we can fit the latest wastewater and hospital 
# admissions data from a single state in Germany for a single time point,
# using wwinference. 

# This is scoping out a potential alternative analysis to evaluate the 
# performance of the wwinference model, with and without wastewater, applied
# to a retrospective forecasting task in an applied public health setting. 

# RKI makes both their hospital admissions and wastewater data (in its raw 
# form) publicly available on Github, with updates at regular intervals. 
# The hospital admissions data is published with both an "actual" 7-day 
# rolling count, and an adjusted count. Presumably, the adjusted count is 
# accounting for right truncation in the real-time data. However, the 
# actual count does not demosntrate characteristic right-truncation artifacts,
# it appears to be systematically lower than the adjusted count throughout
# the time course, perhaps indicating it is not back-filled in this particular
# dataset. 

# We would need to identify an evaluation dataset (e.g. the one that does
# get backfilled), or use one of the two versions as both an input and to 
# evaluate against. Problems for later. 


# Github commit histories could be used to create snapshots of each of the
# data avilable as of historic dates, and we could use these to evaluate
# retrospective forecast performance. 

# We'll start by just fitting a single state. 

# Load in and clean the wastewater and hospital admissions data. 
library(wwinference)
library(dplyr)
library(ggplot2)
library(readr)

RKI_hosp_adj <- readr::read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv") 

hosp_clean <- RKI_hosp_adj |>
  rename(
    date = Datum,
    state = Bundesland,
    age_group = Altersgruppe,
    adj_hosp_7d_count = `fixierte_7T_Hospitalisierung_Faelle`,
    actual_hosp_7d_count = `aktualisierte_7T_Hospitalisierung_Faelle`
  ) |>
  filter(
    #date >= ymd("2025-03-22") - days(100),
         state == "Berlin")

RKI_ww_sites <- readr::read_tsv("https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_einzelstandorte.tsv")

ww_clean <- RKI_ww_sites |>
  dplyr::rename(location = "standort",
                date = "datum",
                state = "bundesland",
                conc = "viruslast",
                pop_cov = "einwohner",
                change_in_lab_indicator = "laborwechsel",
                normalized = "viruslast_normalisiert",
                pathogen = "typ",
                below_LOD = "unter_bg") |>
  dplyr::select(location, date, state, conc, pop_cov, change_in_lab_indicator, normalized, pathogen,
         below_LOD) |>
  filter(state == "BE",
         #date >= ymd("2025-03-22") - days(100),
         pathogen == "SARS-CoV-2")

# Put data into format package expects 
hosp_data <- hosp_clean |>
  rename(daily_hosp_admits = actual_hosp_7d_count) |>
  mutate(state_pop = 3.87e6) |>
  dplyr::select(date,daily_hosp_admits, state_pop)

ww_data <- ww_clean |>
  mutate(
    lab = 1,
    log_genome_copies_per_ml = log((conc/1e3) + 1e-8),
    log_lod = 0.3 # make this up for now (maybe )
  ) |>
  rename(
    site = location,
    site_pop = pop_cov
  ) |>
  dplyr::select(date, site, lab, log_genome_copies_per_ml, log_lod, site_pop, below_LOD) |>
  filter(!is.na(log_genome_copies_per_ml))

# Get parameters and preprocess data 
params <- get_params(
  system.file("extdata", "example_params.toml",
              package = "wwinference"
  )
)

ww_data_preprocessed <- preprocess_ww_data(
  ww_data,
  conc_col_name = "log_genome_copies_per_ml",
  lod_col_name = "log_lod"
)

hosp_data_preprocessed <- preprocess_count_data(
  hosp_data,
  count_col_name = "daily_hosp_admits",
  pop_size_col_name = "state_pop"
)

ggplot(ww_data_preprocessed) +
  geom_point(
    aes(
      x = date, y = log_genome_copies_per_ml,
      color = as.factor(lab_site_name)
    ),
    show.legend = FALSE,
    size = 0.5
  ) +
  geom_point(
    data = ww_data_preprocessed |> filter(
      log_genome_copies_per_ml <= log_lod
    ),
    aes(x = date, y = log_genome_copies_per_ml, color = "red"),
    show.legend = FALSE, size = 0.5
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    labels = scales::date_format("%Y-%m-%d")
  ) +
  geom_hline(aes(yintercept = log_lod), linetype = "dashed") +
  facet_wrap(~lab_site_name, scales = "free") +
  xlab("") +
  ylab("Genome copies/mL") +
  ggtitle("Lab-site level wastewater concentration") +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 5, vjust = 1,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(size = 5),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(
      size = 10,
      vjust = 0.5, hjust = 0.5
    )
  )

ggplot(hosp_data_preprocessed) +
  # Plot the data we will calibrate to
  geom_point(aes(x = date, y = count)) +
  scale_x_date(
    date_breaks = "2 weeks",
    labels = scales::date_format("%Y-%m-%d")
  ) +
  xlab("") +
  ylab("7-day sum of hospital admissions") +
  ggtitle("State level hospital admissions") +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 8, vjust = 1,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(
      size = 10,
      vjust = 0.5, hjust = 0.5
    )
  )

ww_data_to_fit <- indicate_ww_exclusions(
  ww_data_preprocessed,
  outlier_col_name = "flag_as_ww_outlier",
  remove_outliers = TRUE
)

forecast_date <- "2025-03-22"
calibration_time <- 90
forecast_horizon <- 28

generation_interval <- wwinference::default_covid_gi
inf_to_hosp <- wwinference::default_covid_inf_to_hosp
infection_feedback_pmf <- generation_interval

model <- wwinference::compile_model()

# Fit the model with wastewater
ww_fit <- wwinference(
  ww_data = ww_data_to_fit,
  count_data = hosp_data_preprocessed,
  forecast_date = forecast_date,
  calibration_time = calibration_time,
  forecast_horizon = forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = generation_interval,
    inf_to_count_delay = inf_to_hosp,
    infection_feedback_pmf = infection_feedback_pmf,
    params = params
  ),
  fit_opts = list(seed = 123),
  compiled_model = model
)

draws <- get_draws(ww_fit)
hosp_draws_ww_fit <- get_draws(ww_fit, what = "predicted_counts")$predicted_counts
plot_hosp <- get_plot_forecasted_counts(
  draws = draws$predicted_counts,
  forecast_date = forecast_date
)
plot_hosp
plot_ww <- get_plot_ww_conc(draws$predicted_ww, forecast_date)
plot_ww

# Fit to only hospital admissions 
fit_hosp_only <- wwinference(
  ww_data = ww_data_to_fit,
  count_data = hosp_data_preprocessed,
  forecast_date = forecast_date,
  calibration_time = calibration_time,
  forecast_horizon = forecast_horizon,
  model_spec = get_model_spec(
    generation_interval = generation_interval,
    inf_to_count_delay = inf_to_hosp,
    infection_feedback_pmf = infection_feedback_pmf,
    include_ww = FALSE,
    params = params
  ),
  fit_opts = list(seed = 123),
  compiled_model = model
)

draws_hosp_only <- get_draws(fit_hosp_only)$predicted_counts
plot(draws_hosp_only,
     what = "predicted_counts",
     forecast_date = forecast_date
)

#Can try plotting together
hosp_draws_ww_fit <- hosp_draws_ww_fit |>
  mutate(model = "ww + hosp") |>
  left_join(hosp_draws_ww_fit |>
              group_by(date) |>
              summarise(med_pred = quantile(pred_value, 0.5)))
draws_hosp_only <- draws_hosp_only |>
  mutate(model = "hosp") |>
  left_join(draws_hosp_only|>
              group_by(date) |>
              summarise(med_pred = quantile(pred_value, 0.5)))
sampled_draws <- sample.int(2000, 100)
draws_comb <-rbind(hosp_draws_ww_fit, draws_hosp_only) |>
  filter(draw %in% sampled_draws)


ggplot(draws_comb) + 
  geom_line(aes(x = date, y = pred_value, group = draw, color = model),
            alpha = 0.1, linewidth = 0.2) + 
  geom_line(aes(x = date, y = med_pred, color = model)) + 
  geom_point(aes(x = date, y = observed_value), size = 0.8, alpha = 0.5) + 
  geom_vline(xintercept = ymd(forecast_date), linetype ="dashed") +
  theme_bw()  + xlab("") + ylab("7 day hospital admissions") +
  ggtitle("ww+hosp vs hosp only model fit in Berlin") + 
  scale_x_date(
    date_breaks = "2 weeks",
    labels = scales::date_format("%Y-%m-%d")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 8, vjust = 1,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(
      size = 10,
      vjust = 0.5, hjust = 0.5
    )
  )
  

