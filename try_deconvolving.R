sevenday_sum_to_daily_inc <- function(y) {
  # Convert to numeric vector if not already
  y <- as.numeric(y)
  n <- length(y)
  
  # Create identity matrix
  A <- diag(n)
  
  # Add shifted identity matrices for k = 1 to 6
  # This creates upper diagonal matrices shifted by k positions
  for (k in 1:6) {
    # Create a matrix where the diagonal is shifted k positions to the right
    shifted_diag <- matrix(0, nrow = n, ncol = n)
    if (k < n){
      for(i in 1:(n-k)){
        shifted_diag[i, i+k] <- 1
      }
    }
    A <- A + shifted_diag
  }
  
  # Remove last 6 rows
  A <- A[1:(n-6), , drop = FALSE]
  
  # Create initial conditions matrix
  initial_conditions <- cbind(diag(6), matrix(0, nrow = 6, ncol = n-6))
  
  # Combine initial conditions with A
  A <- rbind(initial_conditions, A)
  
  # Solve the system A * x = y
  A_inv <- solve(A)
  x <- A_inv %*% y
  
  return(as.vector(x))
}

#' Convert rolling sums to incidence data
#'
#' @description
#' This function converts k-day rolling sums of count data back to daily 
#' incidence values. It uses the difference between successive rolling sums
#' to recover the incident values, handling the initial period appropriately.
#'
#' @param rolling_sums A numeric vector of k-day rolling sums
#' @param k Integer indicating the number of days that the rolling sums are
#'    computed over. Default is 7. 
#' @param initial_values A numeric vector of the first k-1 incident values 
#'   that contribute to the first rolling sum. If NULL, assumes zeros for the 
#'   initial 6 days.
#'
#' @return A numeric vector of incident values.
#'
#' @details
#' The conversion works as follows:
#' - For the first k days, we use the provided initial values plus 
#'   derived values.
#' - For subsequent days, 
#' incidence[t] = rolling_sum[t] - rolling_sum[t-1] + incidence[t-k]
#' 
#' This is based on the relationship:
#' rolling_sum[t] = sum(daily_incidence[(t-k+1):t])
#'
#' @examples
#' # Example with simple data
#' y <- sample(5, 20, replace = TRUE)
#' rolling_sums <- zoo::rollsum(y_daily, k = 7, na.pad = FALSE)
#' initial_vals <- c(1, 2, 3, 4, 5, 6)
#' daily_incidence <- convert_rolling_sum_to_incidence(rolling_sums, 
#'                                                     k = 7,
#'                                                     initial_vals)
#' @export
convert_rolling_sum_to_incidence <- function(rolling_sums,
                                             k = 7,
                                             initial_values = NULL, 
                                             ) {
  
  # Input validation
  if (length(rolling_sums) == 0) {
    stop("rolling_sums cannot be empty")
  }
  
  if (any(is.na(rolling_sums))) {
    warning("rolling_sums contains NA values. Function expects right-aligned rolling sums") #nolint
  }
  
  # Handle initial values
  if (is.null(initial_values)) {
    # If no initial values provided, assume the first 6 days were zeros
    initial_values <- rep(0, k-1)
    warning("No initial values provided, assuming zeros for the first {k-1} days")
  } else if (length(initial_values) != k-1) {
    stop("initial_values must have exactly {k-1} elements (for days 1-{k-1})")
  }
  
  n_rolling <- length(rolling_sums)
  n_total <- n_rolling + k-1  # Total days including the initial 6
  
  # Initializeincidence vector
  incidence <- numeric(n_total)
  
  # Set the initial k-1 days
  incidence[1:(k-1)] <- initial_values
  
  # Calculate the 7th day from the first rolling sum
  incidence[k] <- rolling_sums[1] - sum(initial_values)
  
  # For days 8 onwards, use the difference formula
  if (n_rolling > 1) {
    for (i in 2:n_rolling) {
      index <- k - 1 + i  # Current day index in the daily_incidence vector
      incidence[index] <- rolling_sums[i] - rolling_sums[i-1] + incidence[index - k]
    }
  }
  
  
  return(incidence)
}


# Test out the function 
y_daily <- sample(5, 20, replace = TRUE)
y_7d_sum <- zoo::rollsum(y_daily, k = 7, na.pad = FALSE)
y <-  y_7d_sum






# Initial values of 7d hospital admissions hospital admissions
df_init <- read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/refs/heads/main/code/resources/initial_values.csv")