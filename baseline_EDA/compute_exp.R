  #' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55
  #' Compute the point forecast:
  #' Arguments:
  #' @param observed the observations / reporting triangle matrix for which to produce nowcasts
  #' @param observed2 the observations / reporting triangle matrix pooled across strata.
  #' This can be used to estimate delay distributions more reliably if they are similar across strata.
  #' Only used if borrow_delays == TRUE.
  #' @param borrow_delays Boolean indicating if the delay distribution be 
  #' estimated from a separate data set observed2
  #' @param n_history integer indicating the number of past observations to use 
  #' to compute point forecast
  #' @param remove_observed Boolean indicating if the available observations
  #' should be removed in the return matrix
  #' @return expectation a matrix normally of the same dimensions as observed 
  #' (possibly padded), but with the expectations added
  compute_exp<- function(observed,
                         observed2 = NULL, 
                         borrow_delays = FALSE, 
                         n_history = 60, 
                         remove_observed = TRUE){
    
    # # turn into matrices:
    # dates <- as.character(observed$date)
    # observed <- as.matrix(observed[, grepl("value", colnames(observed))])
    # rownames(observed) <- dates
    
    if(borrow_delays){
      # catch missing observed2
      if(is.null(observed2)) stop("observed2 is needed if borrow_delays == TRUE")
    }else{
      # setting observed2 to be observed (i.e., delays are estimated from observed)
      # if delay estimation is not to be shared.
      # having a variable observed2 in any case makes the code simpler.
      observed2 <- observed
      if(!is.null(observed2) & !identical(observed, observed2)){
        warning("observed2 is not used as borrow_delays == FALSE")
      } 
    }
    
    # catch too short history: observed2 needs n_history observations
    if(nrow(observed2) < n_history){
      stop("Available reporting triangles are too short for the chosen n_history.")
    }
    # observed can have fewer and may be padded to the right size
    # Need to go back to this bc hard coded to weeks! 
    #observed <- pad_matr(observed, n_history)
    
    # restrict to last n_history observations
    observed <- tail(observed, n_history)
    observed2 <- tail(observed2, n_history) # observed2 should be n_history by max_delay
    nr <- nrow(observed)
    nc <- ncol(observed)
    
    # initialize results matrix:
    expectation <- observed
    # compute expectations iteratively
    for(co in 2:nc){
      # using observed2 to compute multiplication factors:
      block_top_left <- observed2[1:(nr - co + 1), 1:(co - 1), drop = FALSE]
      block_top <- observed2[1:(nr - co + 1), co, drop = FALSE]
      # sum delays across all reference dates for a single delay, divided by 
      # sum across all previous delays across all reference dates 
      factor <- sum(block_top)/max(sum(block_top_left), 1) 
      
      
      # My understanding is this sort of iterates through by column and fills
      # in the bottom right of the reporting triangle, using the observed
      # prop (factor) that the said column is of the previous sums across 
      # columns for the dates that have been reported. This seems reasonable
      # and makes sense, especially if we want to be able to make estimates
      # of the full reporting triangle not just the final sum of the rows
      
      block_left <- expectation[(nr - co + 2):nr, 1:(co - 1), drop = FALSE]
      expectation[(nr - co + 2):nr, co] <- factor*rowSums(block_left)
    }
    # remove the observed values if desired:
    if(remove_observed){
      expectation[!is.na(observed)] <- NA
    }
    
    # return
    return(expectation)
  }