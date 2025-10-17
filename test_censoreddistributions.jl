# Install a bunch of packages we need
import Pkg; Pkg.add("Distributions")
import Pkg; Pkg.add("Plots")

using Pkg; Pkg.add("CensoredDistributions")

# Import the package
using CensoredDistributions
using Distributions
using Random
using Plots

# Set the seed for reproducibility
Random.seed!(123)

# Primary event times generated from a uniform between 0 and 1
primary_event = Uniform(0, 1)

# Delays are generated from a specified delay distribution
dist = LogNormal(1.5, 0.75)

# Plot the distribution of delays
x = 0:0.01:15
plot(x, pdf.(dist, x))

# Total delay = primary event time + delays

prim_dist = primary_censored(dist, primary_event)

# Generate a random sample from the primary distribution 
Random.seed!(123)
rand(prim_dist, 10)

# Plot the CDF compared to the unmodified distribution
x = 0:0.01:15
plot(x, cdf.(dist, x), label="Uncensored")
plot!(x, cdf.(prim_dist, x), label="Primary censored")

# Truncation 

# Filter out delays longer than the observation time. Can apply using the `truncated` function from Distributions.jl

trunc_prim_dist = truncated(prim_dist, upper=10)

# Sample from the truncated distribution
Random.seed!(123)
rand(trunc_prim_dist, 10)

# Plot the CDFs of the different distributions 
x = 0:0.01:15
plot(x, cdf.(dist, x), label="Uncensored")
plot!(x, cdf.(prim_dist, x), label="Primary censored")
plot!(x, cdf.(trunc_prim_dist, x), label="Truncated and primary censored")

# Secondary interval censoring (rounds the truncated delays to the nearest secondary event window)
int_censored_dist = interval_censored(trunc_prim_dist, 1)

# Sample from the distribution 
Random.seed!(123)
rand(int_censored_dist, 10)

# Plot the CDFs of the different distributions
x = 0:0.01:15
plot(x, cdf.(dist, x), label="Uncensored")
plot!(x, cdf.(prim_dist, x), label="Primary censored")
plot!(x, cdf.(trunc_prim_dist, x), label="Truncated and primary censored")
plot!(x, cdf.(int_censored_dist, x), label="Truncated, primary censored, and interval censored")

# Double interval censoring applies primary censoring, truncation, and itnerval censoring in the correct order
# (equivalent to step by step approach above)
double_censored_dist = double_interval_censored(Gamma(2, 1); upper=8, interval=2)

Random.seed!(123)
rand(double_censored_dist, 10)
