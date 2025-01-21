#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L330
#' Get historic version of the reporting triangle as of a given date, for a given
#' age group and location.
#' @param dat_truth data.frame of the reporting triangle
#' @param date character string/date in the format of YYYY-MM-DD indicating 
#' the date that you want the data as of 
#' @param age_group character string indicating the age group for which to get
#' the reporting triangle. Assumed to be a column of `dat_truth`.
#' @param location character string indicating the location. Assumed to be a 
#' column of `dat_truth`.
#' @param weekday_data_updates character indicating the weekday on which updates
#' occur. Necessary to know which updates have already been made at a given 
#' point in time.
#' @param return_matrix Boolean indicating if the reporting triangle be 
#' returned as a matrix instead of a data.frame
#' @return matr Either a matrix with rownames by the reference date and column 
#' names by the delay  or a dataframe with an additional column for the 
#' reference date. Values of mmatrix/dataframe are the number of confirmed 
#' cases as of the date, with NAs for all delays beyond the forecast date.
data_as_of_new <- function(dat_truth, 
                       date, 
                       age_group = "00+", 
                       location = "DE", 
                       weekday_data_updates = "Thursday", 
                       max_lag = NULL,
                       return_matrix = FALSE){
  # check arguments:
  date <- as.Date(date)
  if(!weekdays(date) == weekday_data_updates) warning(paste0("date is usually expected to be a ", weekday_data_updates))

  
  # subset to age group and location
  subs <- dat_truth[dat_truth$age_group == age_group &
                      dat_truth$location == location, ]
  # get matrix containing reporting triangle:
  matr <- subs[, grepl("value_", colnames(subs))]
  # find out which delays the columns correspond to:
  lags_numeric <- get_delays_numeric(col_names = colnames(matr))
  
  # restrict to columns up to max_lag if provided:
  if(!is.null(max_lag)){
    lags_numeric <- lags_numeric[lags_numeric <= max_lag]
    matr <- matr[, paste0("value_", lags_numeric)]
  }
  
  # set up matrix with dates as in data (ends of weeks, typically Sundays)
  matr_dates <- matrix(subs$date, nrow = nrow(matr), ncol = ncol(matr))
  
  # set up matrix with delays corresponding to the columns:
  weekday_data_updates_numeric <- weekday_as_number(weekday_data_updates)
  matr_delays <- matrix(lags_numeric, byrow = TRUE,
                          nrow = nrow(matr), ncol = ncol(matr)) 
  # Removed adding additional delays
  
  # matrix with dates when respective counts became available
  matr_reporting_date <- matr_dates + matr_delays
  matr[as.Date(matr_reporting_date) > date] <- NA
  
  # remove rows with only NAs:
  inds_to_keep <- rowSums(!is.na(matr)) > 0
  matr <- matr[inds_to_keep, ]
  
  if(return_matrix){
    rownames(matr) <- subs$date[inds_to_keep]
    return(matr)
  }else{
    return(data.frame(subs[inds_to_keep,
                           c("location", "age_group","date")],
                      matr))
  }
}
