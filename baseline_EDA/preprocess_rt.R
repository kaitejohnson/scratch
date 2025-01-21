#' Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R
#' Pre-process reporting triangle, without dependency on data being in days 
#' or weeks (assumes no suffix at the end of value_{delay})
#' @param df the reporting triangle as a data.frame, with rows as the reference
#' date and columns as the delay, in any units. Assumes that the delays will
#' be in the format `value_{delay}` with no suffix for the unit
#' @param max_delay the maximum delay, in the delay units, to filter the
#' reporting triangle. Default is 60. 
#' @return trunc_df a dataframe in the same format as `df`, but 
#' truncated to include only the maximum delay number of delay columns
#' and with negative values of reporting handled via passing them to the 
#' subsequent days delay
preprocess_rt <- function(df, max_delay = 60) {
  
  # restrict to the first columns in the max 
  cols_subset <- which(colnames(df) == paste0("value_", max_delay))
  trunc_df <- df[, 1:cols_subset]
  
  # columns containing values:
  value_cols <- which(grepl("value", names(trunc_df)))
  
  # Loop over each row
  for (i in 1:nrow(trunc_df)) {
    to_subtract <- 0
    row <- trunc_df[i, ]
    
    # Loop over the columns starting from the last column back to max delay 
    # column, and if there is a negative value, we add this to the 
    # next day and set that one as 0. 
    for (j in rev(value_cols)) {
      value <- row[[j]]
      
      if (!is.na(value)) {
        # Either adds 0 or the previous days negative value
        value <- value + to_subtract
        
        if (value < 0) {
          # Want to subtract from subsequent day
          to_subtract <- value
          trunc_df[i, j] <- 0 # Set the negative value in the RT to 0 
        } else {
          trunc_df[i, j] <- value
          to_subtract <- 0
        }
      }
    }
  }
  
  # Convert 'value' columns to integer type
  for (col in value_cols) {
    trunc_df[[col]] <- as.integer(trunc_df[[col]])
  }
  
  return(trunc_df)
}