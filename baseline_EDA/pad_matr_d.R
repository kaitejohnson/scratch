#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L112
#' Pad a reporting triangle which is too short.
#' @param matr_observed the reporting triangle
#' @param n_history the minimum number of rows needed
#' @return a padded matrix 
pad_matr_d <- function(matr_observed, n_history){
  if(nrow(matr_observed) < n_history){
    # block of NAs to add
    to_add <- matrix(NA, ncol = ncol(matr_observed),
                     nrow = n_history - nrow(matr_observed))
    colnames(to_add) <- colnames(matr_observed)
    # append
    matr_observed <- rbind(to_add, matr_observed)
    # name rows correctly again
    to <- as.Date(tail(rownames(matr_observed), 1))
    rn <- as.character(seq(to = to, 
                           from = to - (nrow(matr_observed) - 1),
                           by = 1))
    rownames(matr_observed) <- rn
  }
  return(matr_observed)
}