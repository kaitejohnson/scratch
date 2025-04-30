# Example stochastic simulation of independent transmission pairs

# Assumptions:
# 1. Constant R0 with negligible susceptible depletion
# 2. Random variation in individual-day number of contacts (same variation 
# days and between individuals). This introduces variation in the number of
# secondary infections in each individual (to start, later we can scale )
# 3. Number of secondary infections per infector is a function of the number of 
# contacts, with mean equal to the mean number of contacts * prob(inf|contact) = R0  
# and dispersion parameter \phi_C (we could vary this assumption). 
# 4. Number of secondary infections on each day from the infector are a 
# function of the number of contacts on that day, the GI density on that day,
# and the P(inf|contact)

# Step 1: Set R0, GI duration, GI parameters, and mean number of contacts per 
# day and compute P(inf|contact)
# R0 = C_hat*GI_dur*P(inf|contact)

# Step 2: Assign number of contacts per day for N_{trace} days (28)
# C_i(\tau) ~ NegBinom(\mu = C_hat, \phi = \phi_C)

# Step 3: For each N_{trace} day, get the number of expected secondary
# infections
# N_I(\tau) ~ Poisson(\lambda = C_i(\tau)*GI(\tau)*P(inf|contact))