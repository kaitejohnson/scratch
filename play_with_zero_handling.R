triangle <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)
delay_pmf <- get_delay_estimate(
  triangle = triangle,
  max_delay = 3,
  n = 4
)
reporting_square <- apply_delay(
  triangle_to_nowcast = triangle,
  delay_pmf = delay_pmf
)
print(reporting_square)

# One iteration 

expectation <- triangle
co <- 2

n_rows <- nrow(expectation)
block_bottom_left <- expectation[
  max((n_rows - co + 2), 1):n_rows,
  1:(co - 1),
  drop = FALSE
]

exp_total_from_obs <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])

# E[N] = (x + 1 - pi) / pi, where here x = sum of counts already observed
# pi = is cumulative sum of delay pmf up til that point

pi <- sum(delay_pmf[1:(co-1)])
x <- rowSums(block_bottom_left)
exp_N <- (rowSums(block_bottom_left) + 1 - pi)/pi

expectation[max((n_rows - co + 2), 1):n_rows, co] <- exp_N * delay_pmf[co]
  


# second iteration 

co <- 3

block_bottom_left <- expectation[
  max((n_rows - co + 2), 1):n_rows,
  1:(co - 1),
  drop = FALSE
]
exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])
expectation[max((n_rows - co + 2), 1):n_rows, co] <- exp_total * delay_pmf[co]