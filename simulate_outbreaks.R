library(epidist)
growth_rate <- 0.2
obs_time <- 25
outbreak <- simulate_gillespie(r = growth_rate, seed = 101)