library(data.table)
library(httr)

# General pattern
#GET /themes/{theme}/sub_themes/{sub_theme}/topics/{topic}/geography_types/{geography_type}/geographies/{geography}/metrics/{metric}


# Link to data description
#https://ukhsa-dashboard.data.gov.uk/metrics-documentation/measles-cases-by-week-of-symptom-onset

# Topic: Measles
# Category: cases
# API name : measles_cases_casesByOnsetWeek


test_measles_csv <- fread("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/Measles/geography_types/Nation/geographies/England/metrics/measles_cases_casesByOnsetWeek&format=csv", quote = "")
test_measles <- GET("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/Measles/geography_types/Nation/geographies/England/metrics/measles_cases_casesByOnsetWeek")
test_measles$results



# Use the API exmaples and see if they work 
test_covid <- GET("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/COVID-19/geography_types/Nation/geographies/England/metrics/COVID-19_testing_PCRcountByDay")
test_covid_csv <- fread("https://api.ukhsa-dashboard.data.gov.uk/themes/infectious_disease/sub_themes/respiratory/topics/COVID-19/geography_types/Nation/geographies/England/metrics/COVID-19_testing_PCRcountByDay&format=csv", quote = "")
# Data points are supposed to be in `results`
test_covid$results