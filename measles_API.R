library(data.table)
library(httr)

# General pattern
#GET /themes/{theme}/sub_themes/{sub_theme}/topics/{topic}/geography_types/{geography_type}/geographies/{geography}/metrics/{metric}


# Link to data description
#https://ukhsa-dashboard.data.gov.uk/metrics-documentation/measles-cases-by-week-of-symptom-onset

# Topic: Measles
# Category: cases
# API name : measles_cases_casesByOnsetWeek

# Use the API examples: this works
test_covid <- content(GET("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/COVID-19/geography_types/Nation/geographies/England/metrics/COVID-19_testing_PCRcountByDay"))
# Data points are supposed to be in `results`
results_list <- test_covid$results
df <- as.data.frame(do.call(rbind, results_list))

# Try doing the same with measles, filling in the theme and sub themes from above, but swapping theme to outbreaks 
test_measles <- content(GET("https://api.ukhsa-dashboard.data.gov.uk/themes/outbreaks/sub_themes/respiratory/topics/Measles/geography_types/Nation/geographies/England/metrics/measles_cases_casesByOnsetWeek"))
# Go back to infectious diseases 
test2_measles <- content(GET("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/Measles/geography_types/Nation/geographies/England/metrics/measles_cases_casesByOnsetWeek"))
test3_measles <- content(GET("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/Measles/geography_types/Nation/geographies/England/metrics/measles_cases_casesByOnsetWeek"))
results_list <- test_measles$results
df <- as.data.frame(do.call(rbind, results_list))


