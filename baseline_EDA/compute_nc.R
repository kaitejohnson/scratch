#' Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/ee7f15786913cdd887d364d123e29b37ea2a9859/code/baseline/functions.R#L424
#' Generate a nowcast
#' @param observed the observations / reporting triangle data.frame
#' @param location the location for which to generate nowcasts
#' @param age_group the age group for which to generate nowcasts
#' @param forecast_date the date when the nowcast is issued. The function automatically restricts
#' the reportng triangle if an earlier forecast_date is provided to imitate real-time nowcasting.
#' @param type The type of nowcast used, default is `"additions"`
#' @param borrow_delays if borrow_delays == TRUE, a separate reporting triangle observed2 is used to
#' estimate the delay distribution. Typically used to share strength across strata (then observed2,
#' location2, age_group2 corresponds to a pooled reporting triangle).
#' @param borrow_dispersion if borrow_delays == TRUE, the separate reporting triangle is also used to
#' estimate the nowcast dispersion.
#' @param observed2: the observations / reporting triangle matrix pooled across strata.
#' This can be used to estimate delay distributions more reliably if they are similar across strata.
#' @param location2 the location used to estimate the delay distribution (and possibly dispersion).
#' @param age_group2 the age group used to estimate the delay distribution (and possibly dispersion).
#' @param weekday_end_of_week the weekday when data updates happen (needed to know which data were available when).
#' @param max_delay the maximum delay considered.
#' @param n_history_expectations the number of observations used to estimate the delay distribution
#' @param n_history_dispersion the number of re-computed nowcasts used to estimate the error variance
#' @param quantile_levels the predictive quantile levels to return
compute_nc <- function(observed, 
                       location = "DE", 
                       age_group = "00+",
                       forecast_date,
                       type = "additions",
                       borrow_delays = FALSE,
                       borrow_dispersion = FALSE,
                       observed2 = NULL,
                       location2 = NULL,
                       age_group2 = NULL,
                       weekday_data_updates = "Thursday",
                       max_delay = 4, 
                       n_history_expectations = 15, 
                       n_history_dispersion = 15, 
                       quantile_levels = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){
  
  # check reporting triangle and forecast_date:
  if(forecast_date > max(observed$date) + 7){
    stop("Reporting triangle not up to date. forecast_date can be at most max(observed$date) + 7.")
  }
  
  if(any(observed$date >= forecast_date)){
    message("Reporting triangle contains dates later than forecast_date. ", 
            " Note that data will be subsetted to those available on forecast_date (if applicable, negative delays are respected).")
  }
  
  if(weekdays(forecast_date) != weekday_data_updates){
    message("forecast_date is a different weekday than weekday_data_updates. This may be unintended.")
  }
  
  # Can only use observed2 for dispersion if also used for delay distribution:
  if(borrow_dispersion & !borrow_delays){
    stop("borrow_dispersion == TRUE is only allowed of borrow_delays == TRUE")
  }
  
  # Check type is allowed * note could replace with argmatch 
  if(!type %in% c("additions", "revise_average")){
    stop("type needs to be either 'additions' or 'revise_average'.")
  }
  
  # pre-process reporting triangle.
  # note: only done for type == "additions" where nowcasting methods assume positive increments.
  if(type == "additions"){
    observed <- preprocess_rt(observed, max_delay)
    if(!is.null(observed2)){
      observed2 <- preprocess_rt(observed2, max_delay)
    }
  }
  
  # which horizons need to be considered?
  horizons <- get_delays_numeric(col_names = colnames(observed)[grepl("value", colnames(observed))])
  max_horizon <- max_delay - 1 # no need to predict for maximum horizon
  horizons <- horizons[horizons <= max_horizon]
  n_horizons <- length(horizons)
  
  # bring to state as of forecast_date, subset to location and age group.
  # This will make 0s in delays that can't be observed yet NAs. 
  # It will still be in the form of the original reporting triangle units though
  observed_as_of <- data_as_of_new(observed, 
                               age_group = age_group, 
                               location = location,
                               date = forecast_date, 
                               weekday_data_updates = weekday_data_updates,
                               max_lag = max_delay, 
                               return_matrix = TRUE)
  # pad with NAs if needed. Confused here bc can't you double dip and use
  # the same observations to estimate delay and dispersion? 
  # Also this function assumes weekly data 
  # observed_as_of <- pad_matr(observed_as_of, 
  #                            n_history = max(n_history_expectations, 
  #                                            n_history_dispersion)
  #                            )
  

  if(borrow_delays){
    observed2_as_of <- data_as_of(observed2, age_group = age_group2, location = location2, 
                                  date = forecast_date, weekday_data_updates = weekday_data_updates, 
                                  max_lag = max_delay, return_matrix = TRUE)
    # no padding here as observed2_as_of needs to have the desired size
  }else{ # if no sharing desired: set all _2 variables to their regular counterparts
    if(!is.null(observed2) | !is.null(location2) | !is.null(age_group2)){
      warning("observed2, location2 or age_group2 were provided despite borrow_delays == FALSE. These will be ignored.")
    }
    observed2_as_of <- observed_as_of
    location2 <- location
    age_group2 <- age_group
  }
  
  # generate point nowcast:
  point_forecast <- compute_exp(observed = observed_as_of, 
                                observed2 = observed2_as_of,
                                n_history = n_history_expectations, # Not all are complete though 
                                borrow_delays = borrow_delays)
  # This now contains only the bottom right of the reporting triangle, the 
  # top left is NAs. 
  
  # Now modify fit dispersion, since again, this is assuming weekly data...
  # estimate size parameters for negative binomial:
  disp_params <- fit_disp(observed = if(borrow_dispersion) observed2 else observed,
                                location = if(borrow_dispersion) location2 else location,
                                age_group = if(borrow_dispersion) age_group2 else age_group,
                                observed2 = observed2,
                                location2 = location2,
                                age_group2 = age_group2,
                                type = switch(type, "additions" = "size",
                                              "revise_average" = "sd"),
                                forecast_date = forecast_date,
                                max_delay = max_delay,
                                borrow_delays = borrow_delays,
                                borrow_dispersion = borrow_dispersion,
                                n_history_expectations = n_history_expectations,
                                n_history_dispersion = n_history_dispersion,
                                weekday_data_updates = weekday_data_updates)
  
  # bring actual nowcast into standard format, starting at horizon is 0
  mu <- rev(rowSums(point_forecast, na.rm = TRUE))[1:n_horizons] # re-order expecations
  # set up data frame to store:
  df_all <- NULL
  
  # run through horizons:
  for(d in 1:n_horizons){
    # get numeric horizon - only needed in creation of data.frame
    h <- horizons[d]
    
    # by how much do we need to shift quantiles upwards? Note that this needs to use index d
    already_observed <- sum(observed_as_of[nrow(observed_as_of) - d + 1, ], na.rm = TRUE)
    
    # data frame for expecations:
    weekday_data_updates_numeric <- weekday_as_number(weekday_data_updates)
    df_mean <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - weekday_data_updates_numeric - 7*h,
                          horizon = -h,
                          type = "mean",
                          quantile = NA,
                          value = round(mu[d] + already_observed))
    
    # obtain quantiles:
    if(type == "additions"){
      qtls0 <- qnbinom(quantile_levels, 
                       size = disp_params[d], mu = mu[d])
    }else{
      qtls0 <- qnorm(quantile_levels, 
                     sd = disp_params[d], mean = mu[d])
    }
    
    # shift them up by already oberved values
    qtls <- qtls0 + already_observed
    # data.frame for quantiles:
    df_qtls <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - weekday_data_updates_numeric - 7*h,
                          horizon = -h,
                          type = "quantile",
                          quantile = quantile_levels,
                          value = qtls)
    
    # join:
    df <- rbind(df_mean, df_qtls)
    
    # add to results from other horizons
    if(is.null(df_all)){
      df_all <- df
    }else{
      df_all <- rbind(df_all, df)
    }
  }
  
  # return
  return(list(result = df_all,
              mu = mu, size_params = disp_params))# ,
  # expectation_to_add_already_observed = expectation_to_add_already_observed,
  # to_add_already_observed = to_add_already_observed))
}
