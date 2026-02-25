# Isolated benchmark: Diagnosis code vectorization speedup
# This script isolates the performance improvement from JUST vectorizing
# the diagnosis code filtering, independent of the pivot optimization

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
cat("ISOLATED BENCHMARK: DIAGNOSIS CODE VECTORIZATION SPEEDUP\n")
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
    lapply(seq_len(1000), function(i) {
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
# SHARED PREPROCESSING FUNCTION 
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
# FILTERING APPROACH 1: ROW-BY-ROW (ORIGINAL)
# ============================================================================
filter_diagnoses_rowwise <- function(nssp_updates) {
  # Original approach: iterate through each row
  bar_updates <- nssp_updates |>
    filter(map_lgl(diagnoses_codes, ~ any(str_detect(.x, diagnoses_codes_defn))))
  
  return(nrow(bar_updates))
}

# ============================================================================
# FILTERING APPROACH 2: VECTORIZED (OPTIMIZED)
# ============================================================================
filter_diagnoses_vectorized <- function(nssp_updates) {
  # Optimized approach: single regex pattern, vectorized
  diagnosis_pattern <- paste(diagnoses_codes_defn, collapse = "|")
  bar_updates <- nssp_updates |>
    filter(str_detect(diagnoses_codes, diagnosis_pattern))
  
  return(nrow(bar_updates))
}

# ============================================================================
# BENCHMARKING
# ============================================================================
results <- data.frame(
  dataset = character(),
  n_rows = integer(),
  preprocessing_time = numeric(),
  rowwise_filtering_time = numeric(),
  vectorized_filtering_time = numeric(),
  stringsAsFactors = FALSE
)

cat("Running isolated benchmarks...\n\n")

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  n_rows <- nrow(dataset)
  
  cat("Testing", dataset_name, "dataset (", n_rows, "rows)...\n")
  
  # Step 1: Preprocess (timed separately to show it's not the bottleneck being tested)
  cat("  Preprocessing to nssp_updates...")
  # profvis({
  #   nssp_updates <- preprocess_to_nssp_updates(dataset)
  #   result <- filter_diagnoses_rowwise(nssp_updates)
  #   result2 <- filter_diagnoses_vectorized(nssp_updates)
  # })
  preprocess_times <- replicate(3, {
    system.time({
      nssp_updates <- preprocess_to_nssp_updates(dataset)
      
    })["elapsed"]
  })
  preprocess_time <- mean(preprocess_times)
  cat(" Done! (", round(preprocess_time, 4), "s avg)\n")
  
  # Create nssp_updates once for filtering tests
  nssp_updates <- preprocess_to_nssp_updates(dataset)
  
  # Step 2: Benchmark row-by-row filtering
  cat("  Row-by-row filtering...")
  rowwise_times <- replicate(5, {
    system.time({
      result <- filter_diagnoses_rowwise(nssp_updates)
    })["elapsed"]
  })
  rowwise_time <- mean(rowwise_times)
  cat(" Done! (", round(rowwise_time, 4), "s avg)\n")
  
  # Step 3: Benchmark vectorized filtering
  cat("  Vectorized filtering...")
  vectorized_times <- replicate(5, {
    system.time({
      result <- filter_diagnoses_vectorized(nssp_updates)
    })["elapsed"]
  })
  vectorized_time <- mean(vectorized_times)
  cat(" Done! (", round(vectorized_time, 4), "s avg)\n\n")
  
  # Store results
  results <- rbind(results,
                   data.frame(
                     dataset = dataset_name,
                     n_rows = n_rows,
                     preprocessing_time = preprocess_time,
                     rowwise_filtering_time = rowwise_time,
                     vectorized_filtering_time = vectorized_time
                   )
  )
}

#Profiling for large dataset
profvis({
  nssp_updates <- preprocess_to_nssp_updates(dataset)
  result <- filter_diagnoses_rowwise(dataset)
  result2 <- filter_diagnoses_vectorized(dataset)
})

# ============================================================================
# RESULTS
# ============================================================================
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DIAGNOSIS VECTORIZATION SPEEDUP RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Calculate speedup
results <- results |>
  mutate(
    filtering_speedup = rowwise_filtering_time / vectorized_filtering_time,
    filtering_improvement_pct = (rowwise_filtering_time - vectorized_filtering_time) / rowwise_filtering_time * 100,
    filtering_time_saved = rowwise_filtering_time - vectorized_filtering_time
  )

cat("ISOLATED DIAGNOSIS FILTERING PERFORMANCE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf("%-10s | %6s | %10s | %10s | %8s | %10s\n",
            "Dataset", "Rows", "Row-by-Row", "Vectorized", "Speedup", "Saved (s)"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  row <- results[i, ]
  cat(sprintf("%-10s | %6d | %8.4fs | %8.4fs | %6.1fx | %8.4fs\n",
              row$dataset,
              row$n_rows,
              row$rowwise_filtering_time,
              row$vectorized_filtering_time,
              row$filtering_speedup,
              row$filtering_time_saved))
}

cat(paste(rep("-", 80), collapse = ""), "\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("• Vectorization speedup on small dataset  (25 rows):   %.1fx\n",
            results$filtering_speedup[results$dataset == "small"]))
cat(sprintf("• Vectorization speedup on medium dataset (500 rows):  %.1fx\n",
            results$filtering_speedup[results$dataset == "medium"]))
cat(sprintf("• Vectorization speedup on large dataset  (2500 rows): %.1fx\n",
            results$filtering_speedup[results$dataset == "large"]))
cat("\n")

cat("INTERPRETATION:\n")
cat("This benchmark isolates ONLY the diagnosis code filtering step.\n")
cat("The preprocessing (string parsing) is identical for both approaches.\n")
cat("Speedup shows the improvement from vectorized str_detect() vs map_lgl().\n")
cat("\n")

cat("Note: Row-by-row filtering time increases with:\n")
cat("  1. Number of records to filter\n")
cat("  2. Number of diagnosis codes in the pattern\n")
cat("  3. Complexity of regex matching\n")
cat("\n")

# Save results
write.csv(results, "diagnosis_vectorization_results.csv", row.names = FALSE)
cat("✓ Results saved to: diagnosis_vectorization_results.csv\n")