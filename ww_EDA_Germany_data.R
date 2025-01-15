RKI_ww <- readr::read_tsv("https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_aggregierte_kurve.tsv")

RKI_ww_clean <- RKI_ww |>
  dplyr::rename(date = "datum",
                pop_cov = "anteil_bev",
                conc = "viruslast",
                pathogen ="typ") |>
  dplyr::select(date, pop_cov, pathogen, conc) 

ggplot(RKI_ww_clean) + geom_line(aes(x = date, y = conc, color = pathogen))+
  scale_y_continuous(trans = "log10")
  

RKI_ww_sites <- readr::read_tsv("https://raw.githubusercontent.com/robert-koch-institut/Abwassersurveillance_AMELAG/refs/heads/main/amelag_einzelstandorte.tsv")

RKI_sites_clean <- RKI_ww_sites |>
  dplyr::rename(location = "standort",
                date = "datum",
                state = "bundesland",
                conc = "viruslast",
                pop_cov = "einwohner",
                change_in_lab_indicator = "laborwechsel",
                pathogen = "typ",
                below_LOD = "unter_bg") |>
  select(location, date, state, conc, pop_cov, change_in_lab_indicator, pathogen,
         below_LOD) 

ggplot(RKI_sites_clean |> dplyr::filter(pathogen == "SARS-CoV-2")) + 
  geom_point(aes(x = date, y = conc, color = location),
             show.legend = FALSE, size = 0.5) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~state) +
  theme_bw()

ggplot(RKI_sites_clean |> dplyr::filter(pathogen == "Influenza A+B")) + 
  geom_point(aes(x = date, y = conc, color = location), show.legend = FALSE,
             size = 0.5) +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~state) + theme_bw()

#Hospitalization data 
RKI_hosp_triangle <- readr::read_csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/data-truth/COVID-19/COVID-19_hospitalizations.csv")
RKI_hosp_by_report <- readr::read_csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/data-truth/COVID-19/COVID-19_hospitalizations_by_reporting.csv")

RKI_hosp  <- RKI_hosp_by_report |> 
  dplyr::filter(age_group == "00+") 

ggplot(RKI_hosp |> dplyr::filter(location != "DE")) + 
  geom_point(aes(x = date, y = value), size = 0.5) +
  facet_wrap(~location, scales = "free_y")

ggplot(RKI_hosp_triangle)