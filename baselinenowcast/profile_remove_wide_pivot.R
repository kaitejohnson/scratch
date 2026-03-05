# Isolated benchmark: Removing pivot from long to wide back to long

library(baselinenowcast)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(data.table)

# Load data
data("syn_nssp_line_list")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ISOLATED BENCHMARK: REMOVE WIDE PIVOT SPEEDUP\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Create test datasets
cat("Creating test datasets...\n")
datasets <- list(
  small = syn_nssp_line_list,  # 25 rows
  medium = bind_rows(
    lapply(seq_len(200), function(i) {
      syn_nssp_line_list |>
        mutate(
          replicate_id = i,
          C_Processed_BioSense_ID = paste0(C_Processed_BioSense_ID, "_rep", i)
        )
    })
  ),
  large = bind_rows(
    lapply(seq_len(5000), function(i) {
      syn_nssp_line_list |>
        mutate(
          replicate_id = i,
          C_Processed_BioSense_ID = paste0(C_Processed_BioSense_ID, "_rep", i)
        )
    })
  )
)

cat("Dataset sizes:\n")
cat("  Small:", nrow(datasets$small), "rows\n")
cat("  Medium:", nrow(datasets$medium), "rows\n")

# Define diagnosis codes
diagnoses_codes_defn <- c("U071", "U07.1", "J00", "J06")  # Simplified for testing


# ============================================================================
# LONG TO WIDE TO LONG PREPROCESSING FUNCTION 
# ============================================================================
preprocess_to_nssp_updates <- function(syn_nssp_line_list) {
  expand_events <- function(line_list, event_col_name) {
    wide_line_list <- separate_wider_delim(line_list,
                                           {{ event_col_name }},
                                           delim = "{", names_sep = "", too_few = "align_start"
    )
    return(wide_line_list)
  }
  
  wide_to_long <- function(wide_line_list,
                           event_col_name,
                           values_to,
                           names_to,
                           id_col_name) {
    long_data <- wide_line_list |>
      pivot_longer(
        cols = starts_with({{ event_col_name }}),
        names_to = {{ names_to }},
        values_to = {{ values_to }},
        values_drop_na = FALSE
      ) |>
      mutate(
        event_id = paste(
          .data[[id_col_name]],
          as.numeric(str_extract(as.character(.data[[names_to]]), "[0-9.]+"))
        )
      )
    return(long_data)
  }
  
  syn_nssp_time_stamps_wide <- expand_events(
    line_list = syn_nssp_line_list,
    event_col_name = "DischargeDiagnosisMDTUpdates"
  ) |>
    select(-DischargeDiagnosisUpdates)
  
  syn_nssp_diagnoses_wide <- expand_events(
    line_list = syn_nssp_line_list,
    event_col_name = "DischargeDiagnosisUpdates"
  ) |>
    select(-DischargeDiagnosisMDTUpdates)
  
  syn_nssp_time_stamps_long <- wide_to_long(
    wide_line_list = syn_nssp_time_stamps_wide,
    event_col_name = "DischargeDiagnosisMDTUpdates",
    values_to = "time_stamp",
    names_to = "column_name",
    id_col_name = "C_Processed_BioSense_ID"
  )
  
  syn_nssp_diagnoses_long <- wide_to_long(
    wide_line_list = syn_nssp_diagnoses_wide,
    event_col_name = "DischargeDiagnosisUpdates",
    values_to = "diagnoses_codes",
    names_to = "column_name",
    id_col_name = "C_Processed_BioSense_ID"
  )
  
  # Clean timestamps
  syn_nssp_time_stamps <-
    syn_nssp_time_stamps_long |>
    mutate(
      time_stamp = as.POSIXct(
        gsub(
          "[|;]+", "",
          sub(".*?\\}", "", time_stamp)
        ),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      C_Visit_Date_Time = as.POSIXct(C_Visit_Date_Time)
    ) |>
    drop_na(time_stamp)
  
  # Clean diagnoses
  syn_nssp_diagnoses <-
    syn_nssp_diagnoses_long |>
    mutate(
      diagnoses_codes = gsub("^[0-9]+\\};;", "", diagnoses_codes),
      diagnoses_codes = gsub("[|;]+$", "", diagnoses_codes)
    ) |>
    filter(nzchar(diagnoses_codes)) |>
    filter(!is.na(diagnoses_codes)) |>
    select(event_id, diagnoses_codes)
  
  # Merge and filter
  nssp_merged <- syn_nssp_time_stamps |>
    left_join(syn_nssp_diagnoses,
              by = "event_id"
    ) |>
    filter(!is.na(diagnoses_codes))
  
  # Add delay
  nssp_updates <- nssp_merged |>
    mutate(arrival_to_update_delay = as.numeric(difftime(
      time_stamp, C_Visit_Date_Time,
      units = "days"
    )))
  
  return(nssp_updates)
}

# ============================================================================
# UNNEST PREPROCESSING FUNCTION 
# ============================================================================
preprocess_using_unnest <- function(syn_nssp_line_list) {
syn_nssp_long <- syn_nssp_line_list |>
  mutate(
    time_stamp = str_split(DischargeDiagnosisMDTUpdates, "\\{"),
    diagnoses_codes = str_split(DischargeDiagnosisUpdates, "\\{")
  ) |>
  select(-DischargeDiagnosisMDTUpdates, -DischargeDiagnosisUpdates) |>
  unnest(cols = c(time_stamp, diagnoses_codes))

syn_nssp_clean <- syn_nssp_long |>
  mutate(
    time_stamp = as.POSIXct(
      str_remove_all(str_remove(time_stamp, ".*\\}"), "[|;]+"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    diagnoses_codes = str_remove(diagnoses_codes, ".*\\}")
  ) |>
  filter(!is.na(time_stamp), nzchar(diagnoses_codes), diagnoses_codes != ";;|")

nssp_updates <- syn_nssp_clean |>
  mutate(arrival_to_update_delay = as.numeric(difftime(
    time_stamp, C_Visit_Date_Time,
    units = "days"
  )))
return(nssp_updates)
}

# ============================================================================
# BENCHMARKING
# ============================================================================
results <- data.frame(
  dataset = character(),
  n_rows = integer(),
  preprocessing_wide_to_long_time = numeric(),
  preprocessing_unnest_time = numeric(),
  stringsAsFactors = FALSE
)

cat("Running isolated benchmarks...\n\n")

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  n_rows <- nrow(dataset)
  
  cat("Testing", dataset_name, "dataset (", n_rows, "rows)...\n")
  
  cat("  Preprocessing to nssp_updates...")
  preprocessing_wide <- replicate(3, {
    system.time({
      nssp_updates <- preprocess_to_nssp_updates(dataset)
      
    })["elapsed"]
  })
  preprocess_wide_time <- mean(preprocessing_wide)
  cat(" Done! (", round(preprocess_wide_time, 4), "s avg)\n")
  
  preprocessing_unnest <- replicate(3, {
    system.time({
      nssp_updates <- preprocess_using_unnest(dataset)
      
    })["elapsed"]
  })
  preprocess_unnest_time <- mean(preprocessing_unnest)
  cat(" Done! (", round(preprocess_unnest_time, 4), "s avg)\n")
  
  
  # Store results
  results <- rbind(results,
                   data.frame(
                     dataset = dataset_name,
                     n_rows = n_rows,
                     preprocessing_wide_to_long_time = preprocess_wide_time,
                     preprocessing_unnest_time = preprocess_unnest_time
                   )
  )
}

# Calculate speedup
results <- results |>
  mutate(
   speedup = preprocessing_wide_to_long_time / preprocessing_unnest_time,
    improvement_pct = (preprocessing_wide_to_long_time - preprocessing_unnest_time) / preprocessing_wide_to_long_time * 100,
    time_saved = preprocessing_wide_to_long_time - preprocessing_unnest_time
  )