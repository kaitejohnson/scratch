import Pkg; Pkg.add("DataFramesMeta")
import Pkg; Pkg.add("Turing")
import Pkg; Pkg.add("DynamicPPL")
import Pkg; Pkg.add("Random")
import Pkg; Pkg.add("CairoMakie")
import Pkg; Pkg.add("StatsBase")
import Pkg; Pkg.add("PairPlots")

using DataFramesMeta
using Turing
using DynamicPPL
using Distributions
using Random
using CairoMakie, PairPlots
using StatsBase
using CensoredDistributions

# We'll simulate from the Turing model with known parameters
Random.seed!(123) # Set seed for reproducibility

# Define true parameters for generating synthetic Data
n = 2000;
meanlog = 1.5;
sdlog = 0.75;

# Define a lognormal using Distributions.jl
true_dist = LogNormal(meanlog, sdlog);

# For each individual, we sample a primary and secondary event window as well as a relative observation time (relative to their censored primary event)

# Define a resuable submlodel for the latent delay distribution 
# Allows us to reuse the same prior structure across all our models 

@model function latent_delay_dist()
    mu ~ Normal(1.0, 2.0);
    sigma ~ truncated(Normal(0.5, 1); lower = 0.0)
    return LogNormal(mu, sigma)
end

# And define a helper function to standardize our pairplot visualization across model fits
function plot_fit_with_truth(chain, truth_dict)
    f = pairplot(
        chain,
        PairPlots.Truth(
            truth_dict,
            label = "True Values"
            )
    )
    return f
end 

# Prior predictive checks using pairplot

latent_prior_samples = sample(latent_delay_dist(), Prior(), 1000)
plot_fit_with_truth(latent_prior_samples, (; mu = meanlog, sigma = sdlog))

# Define the double censored model for simulation and fitting 
@model function CensoredDistributions_model(pwindow_bounds, swindow_bounds, obs_time_bounds)
    pwindows ~ arraydist([DiscreteUniform(pw[1], pw[2]) for pw in pwindow_bounds])
    swindows ~ arraydist([DiscreteUniform(sw[1], sw[2]) for sw in swindow_bounds])
    obs_times ~ arraydist([DiscreteUniform(ot[1], ot[2]) for ot in obs_time_bounds])

    dist ~ to_submodel(latent_delay_dist())

    pcens_dists = map(pwindows, obs_times, swindows) do pw, D, sw
        pe = Uniform(0, pw)
        double_interval_censored(
            dist; primary_event = pe, upper = D, interval = sw
        )
    end # End function 

    obs ~ weight(pcens_dists)
end  # End model

# Define simulated observation window bounds for each observed delay

bounds_df = DataFrame(
    pwindow_bounds = fill((1,3), n), # Each observaton cna have pwindow 1-3
    swindow_bounds = fill((1,3), n), # Each observation can have swindow 1-3
    obs_time_bounds = fill((8,12), n) # Each observation can have obs_time 8-12
)

# Simulate from the double censored distribution for each individual
# Sample from the model using known true parameters using DynamicPPL's fix function to set 
# parameters to their true valies and sample from the prior predictive distribution.

# First define a set of primary event distributions, then define the model using observation windows

model_for_simulation = @with bounds_df begin
    CensoredDistributions_model(:pwindow_bounds, :swindow_bounds, :obs_time_bounds)
end

# Create the base model (unfixed)  to be used for both simulation and fitting
base_model = @with bounds_df begin 
    CensoredDistributions_model(:pwindow_bounds, :swindow_bounds, :obs_time_bounds)
end

# For simulation, fix the distribution parameters to known true values 
simulation_model = fix(
    base_model,
    (
        @varname(dist.mu) => meanlog,
        @varname(dist.sigma) =>sdlog
    )
)

# Sample from the model using `rand` to get simulation observations with their observation 
# windows and relative observation time 
simulated_data = @chain simulation_model begin 
    rand
    DataFrame
end

# Create a dataframe with the data we just generated, aggregated to unique combinations and count occurences
simulated_counts = @chain simulated_data begin
    @transform :obs_upper = :obs .+ :swindows
    @groupby All()
    @combine :n = length(:pwindows)
end

# Compare samples with and without double interval censoring to the true distribution. First calculate empirical cdf
empirical_cdf_obs = @with(simulated_counts, ecdf(:obs, weights = :n));
# Create a sequence of x values for the theoretical CDF
x_seq = @with simulated_counts begin
    range(minimum(:obs), stop = maximum(:obs) + 2, length = 100);
end


begin
    # Calculate theoretical CDF using true log-normal distribution
    theoretical_cdf = @chain x_seq begin
        cdf.(true_dist, _)
    end;

    # Generate uncensored samples from the true distribution for comparison
    uncensored_samples = rand(true_dist, n);
    empirical_cdf_uncensored = ecdf(uncensored_samples);
end

let
    f = Figure()
    ax = Axis(f[1, 1],
        title = "Comparison of Censored vs Uncensored vs Theoretical CDF",
        ylabel = "Cumulative Probability",
        xlabel = "Delay"
    )
    CairoMakie.scatter!(
        ax,
        x_seq,
        empirical_cdf_obs.(x_seq),
        label = "Empirical CDF (Censored)",
        color = :blue
    )
    CairoMakie.scatter!(
        ax,
        x_seq,
        empirical_cdf_uncensored.(x_seq),
        label = "Empirical CDF (Uncensored)",
        color = :red,
        marker = :cross
    )
    lines!(ax, x_seq, theoretical_cdf, label = "Theoretical CDF",
        color = :black, linewidth = 2)
    vlines!(
        ax, [mean(simulated_data.obs)], color = :blue, linestyle = :dash,
        label = "Censored mean", linewidth = 2)
    vlines!(ax, [mean(uncensored_samples)], color = :red, linestyle = :dash,
        label = "Uncensored mean", linewidth = 2)
    vlines!(ax, [mean(true_dist)], linestyle = :dash,
        label = "Theoretical mean", color = :black, linewidth = 2)
    axislegend(position = :rb)

    f
end

# Fitting a model using Turing

# Start with a naive model ignoring the censoring process. Treats the observed delay data as if it came directly from the uncensored elay distribution.

@model function naive_model()
    dist ~ to_submodel(latent_delay_dist())
    obs ~ weight(dist)
end

# Instantiate and condition this model using weighted observations. Condition directly using the NamedTuple format (values = values, weights = counts) which enables joint observation conditioning.

naive_mdl = @with simulated_counts begin
    condition(naive_model(), obs = (values = :obs .+ 1e-6, weights = :n))
end

# Fit the conditioned model using the joint observation pattern (values = values, weights = counts)

naive_fit = sample(naive_mdl, NUTS(), MCMCThreads(), 500, 4); 

summarize(naive_fit)

plot_fit_with_truth(naive_fit, Dict("dist.mu" => meanlog, "dist.sigma" => sdlog))

#Fit a truncation -adjusted interval model

@model function interval_only_model(swindow_bounds, obs_time_bounds)
    swindows ~ arraydist([Uniform(sw[1], sw[2]) for sw in swindow_bounds])
    obs_times ~ arraydist([Uniform(ot[1], ot[2]) for ot in obs_time_bounds])
    
    dist ~ to_submodel(latent_delay_dist())

    icens_dists = map(obs_times, swindows) do D, sw
        truncated(interval_censored(dist, sw), upper = D)
    end
    obs ~ weight(icens_dists)
    return obs
end 

# Create interval only model with bounds, fix window parameters, and condition on observations
# This is the equivalent I think of setting the stan data (so we tell it what the windows are here)
interval_only_mdl = @with simulated_counts begin
    @chain interval_only_model(bounds_df.swindow_bounds, bounds_df.obs_time_bounds) begin
        fix((
            @varname(swindows) => :swindows,
            @varname(obs_times) => :obs_times
        ))
        condition(obs = (values = :obs, weights = :n))
    end
end;

# Fit the interval only model
interval_only_fit = sample(interval_only_mdl, NUTS(), MCMCThreads(), 500, 4);
summarize(interval_only_fit)

plot_fit_with_truth(interval_only_fit, Dict("dist.mu" => meanlog, "dist.sigma" =>sdlog))

# Now fit the double censored model 
CensoredDistributions_mdl = @with simulated_counts begin 
    @chain base_model begin 
        fix((
            @varname(pwindows) => :pwindows,
            @varname(swindows) => :swindows,
            @varname(obs_times) => :obs_times
        ))
        condition(obs = (values = :obs, weights = :n))
    end
end; 

# Fit the model 
CensoredDistributions_fit = sample(
    CensoredDistributions_mdl, NUTS(), MCMCThreads(), 1000,4);
summarize(CensoredDistributions_fit)

plot_fit_with_truth(
    CensoredDistributions_fit, Dict("dist.mu" => meanlog, "dist.sigma" => sdlog))