#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L135
#' Fit dispersion parameters:
#' - either size of a negative binomial 
#' - or sd of a normal
#' this is based on historical point nowcasts and observations
#' @param observed the observations / reporting triangle data.frame
#' @param location the location for which to generate nowcasts
#' @param age_group the age group for which to generate nowcasts
#' @param forecast_date the date when the nowcast is issued. The function automatically restricts
#' the reportng triangle if an earlier forecast_date is provided to imitate real-time nowcasting.
#' @param type The type of dispersion parameter to estimate, default is "size"
#' @param borrow_delays if borrow_delays == TRUE, a separate reporting triangle observed2 is used to
#' estimate the delay distribution. Typically used to share strength across strata (then observed2,
#' location2, age_group2 corresponds to a pooled reporting triangle).
#' @param borrow_dispersion if borrow_delays == TRUE, the separate reporting triangle is also used to
#' estimate the nowcast dispersion.
#' @param observed2: the observations / reporting triangle matrix pooled across strata.
#' This can be used to estimate delay distributions more reliably if they are similar across strata.
#' @param location2 the location used to estimate the delay distribution (and possibly dispersion).
#' @param age_group2 the age group used to estimate the delay distribution (and possibly dispersion).
#' @param weekday_data_updates the weekday when data updates happen (needed to know which data were available when).
#' @param max_delay the maximum delay considered.
#' @param n_history_expectations the number of observations used to estimate the delay distribution
#' @param n_history_dispersion the number of re-computed nowcasts used to estimate the error variance
fit_disp <- function(observed, 
                     location, 
                     age_group,
                     observed2 = NULL,
                     location2 = NULL,
                     age_group2 = NULL,
                     type = "size",
                     forecast_date, 
                     max_delay,
                     borrow_delays = FALSE,
                     borrow_dispersion = FALSE,
                     n_history_expectations,
                     n_history_dispersion,
                     weekday_data_updates = "Thursday"){
  
  if(borrow_delays){
    # catch missing observed2
    if(is.null(observed2)) stop("observed2 is needed if borrow_delays == TRUE")
  }else{
    if(!is.null(observed2)) warning("observed2 is not used as borrow_delays == FALSE")
    observed2 <- observed
  }
  
  if(!type %in% c("size", "sd")){
    stop("type needs to be either size or sd.")
  }
  
  # if no observed2 is provided: use observed
  # having an object observed2 in any case makes the code simpler in the following
  if(is.null(observed2)){
    observed2 <- observed
  }
  
  
  # bring reporting triangles to state as of forecast date:
  matr_observed <- data_as_of_new(observed, 
                              age_group = age_group, 
                              location = location, 
                              date = forecast_date,
                              weekday_data_updates = weekday_data_updates, 
                              max_lag = max_delay, 
                              return_matrix = TRUE)
  # pad matr_observed with NAs if too short:
  #matr_observed <- pad_matr(matr_observed, n_history_expectations + n_history_dispersion)
  matr_observed2 <- data_as_of_new(observed2, 
                                   age_group = age_group2, 
                                   location = location2, 
                                   date = forecast_date,
                                   weekday_data_updates = weekday_data_updates, 
                                   max_lag = max_delay, 
                                   return_matrix = TRUE)
  # no padding here as matr_observed2 cannot be too short.
  
  # check length of available reporting triangles.
  if(nrow(observed2) < n_history_expectations + n_history_dispersion){
    stop("Available reporting triangles are too short for the chosen n_history_expectations and n_history_dispersion.")
  }
  if(!borrow_delays & nrow(observed) < n_history_expectations){
    stop("Available reporting triangles are too short for the chosen n_history_expectations and n_history_dispersion.")
  }
  
  # generate point forecasts for n_history_dispersion preceding weeks
  # these are necessary to estimate dispersion parameters
  # determine dates
  if(type == "size"){
    # for size parameters there is a smart way of using partial observations
    all_forecast_dates <- seq(from = forecast_date - (n_history_dispersion) + 1, by = 1,
                              to = forecast_date - 1) # exclude actual forecast date
  }else{
    # for sds incomplete observations need to be excluded
    all_forecast_dates <- seq(from = forecast_date - 7*(n_history_dispersion), by = 7,
                              to = forecast_date - (max_delay - 1)*7)
  }
  
  
  # set up matrices to store results.
  # Note, very stuck here, this is not currently working -- in particular,
  # the point estimate isn't working because we pad the data with NAs at the 
  # top and we need the top part to make the point estimate... 
  # each of these contains the forecast dates in the rows and horizons in the columns
  # (i.e., columns are not delays, but horizons, and each cell contains a total
  # value corresponding to that horizon, e.g., the total expected value to add)
  n_horizons <- ncol(matr_observed) - 1
  expectation_to_add <- # full expectations of counts to add
    expectation_to_add_already_observed <- # expectations of the sum over already observable quantities
    to_add_already_observed <- # sums over the respective observed quantities
    matrix(NA, nrow = length(all_forecast_dates), ncol = n_horizons,
           dimnames = list(as.character(all_forecast_dates), NULL))
  
  # run through forecast dates to generate point nowcasts and corresponding observations:
  for(t in seq_along(all_forecast_dates)){
    # identify date for which to compute retrospective nowcast
    forecast_date_temp <- all_forecast_dates[t]
    # This truncates to only the reference dates before the temp forecast date
    # and only the delays we would have observed before the forecast date 
    matr_observed_temp <- data_as_of_new(observed, age_group = age_group, location = location, date = forecast_date_temp,
                                     weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
    matr_observed2_temp <- data_as_of_new(observed2, age_group = age_group2, location = location2, date = forecast_date_temp,
                                      weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
    
    # pad and catch case where matr_observed does not contain any data
    if(nrow(matr_observed_temp) > 0){
      # This becomes buffered on top (earlier dates) with NAs so you have a 
      # full history of expectations. Problem is point forecast here will be NAs... 
      matr_observed_temp <- pad_matr_d(matr_observed_temp, n_history_expectations)
    }else{
      matr_observed_temp <- NA*matr_observed2_temp
    }
    
    # get same subset of the reporting triangle, but filled as far as possible at forecast_date:
    # So this is filled in with the values that were actually observed, whereas 
    # matr_obs_temp contains NAs to mimick what was known as of the temp forecast date
    matr_observed_temp_full <- matr_observed[which(rownames(matr_observed) %in% 
                                                     tail(rownames(matr_observed_temp), 
                                                          n_history_expectations)), ]
    # this is needed to estimate dispersion parameters below
    
    # generate retrospective point nowcast using the temp forecast date data to
    # fill in to get the complete expectations to compare to `matr_observed_temp_full`
    point_forecasts_temp <- compute_exp(observed = matr_observed_temp,
                                        observed2 = matr_observed2_temp,
                                        n_history = n_history_expectations,
                                        borrow_delays = borrow_delays, 
                                        remove_observed = TRUE)
    
    # structure by things already observed or not (necessary to use of partial observations)
    for(d in 1:n_horizons){
      # which indices in the matrix correspond to nowcasts at horizon d?
      # This will be 
      inds_nowc <- indices_nowcast(matr_observed_temp, d = d - 1, w = 1,
                                   n_history_expectations = n_history_expectations)
      # compute sum of expected values for nowcast at horizon d over all elements of the reporting
      # triangle which have not yet been observed at time forecast_date_temp
      expectation_to_add[t, d] <- sum(point_forecasts_temp*inds_nowc, na.rm = TRUE)
      
      # which indices of the reporting triangle are already known at forecast_date?
      # (the one from the original function call, not forecast_date_temp)
      inds_already_observed <- !is.na(matr_observed_temp_full)
      
      # compute sum of expected values for nowcast at horizon d over all elements of the reporting
      # triangle which have not yet been observed at time forecast_date_temp, but have been observed at forecast_date
      expectation_to_add_already_observed[t, d] <- sum(point_forecasts_temp*inds_already_observed*inds_nowc, na.rm = TRUE)
      # compute the corresponding observed values
      to_add_already_observed[t, d] <- sum(matr_observed_temp_full*inds_already_observed*inds_nowc, na.rm = TRUE)
    }
  }
  
  # estimate dispersion
  disp_params <- numeric(ncol(expectation_to_add))
  # remove rows with zero initial reports (Christmas etc)
  to_keep <- abs(expectation_to_add_already_observed[, 1]) >= 0.1
  # The to_add_already_observed is any cases that would be added to the cumulative total cases,
  # as of any horizon at a particular forecast date. 
  to_add_already_observed <- to_add_already_observed[to_keep, ]
  expectation_to_add_already_observed <- expectation_to_add_already_observed[to_keep, ]
  
  if(nrow(to_add_already_observed) == 0){
    stop("Cannot estimate nowcast dispersion from available data. Maybe try borrow_dispersion = TRUE.")
  }
  
  # run through horizons
  for(i in 1:n_horizons){
    obs_temp <- to_add_already_observed[, i]
    mu_temp <- expectation_to_add_already_observed[, i]
    if(type == "size"){
      mu_temp <- mu_temp + 0.1
      # plus 0.1 to avoid ill-defined negative binomial
      disp_params[i] <- fit_nb(x = obs_temp, mu = mu_temp)
    }else{
      disp_params[i] <- sd(obs_temp - mu_temp)
    }
  }
  
  return(disp_params)
}