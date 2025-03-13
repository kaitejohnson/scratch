library(fitdistrplus)
library(primarycensored)

set.seed(123) # For reproducibility

# Define the number of samples to generate
n <- 1000

# Define the true distribution parameters
shape <- 1.77 # This gives a mean of 4 and sd of 3 for a gamma distribution
rate <- 0.44

# Generate fixed pwindow, swindow, and obs_time
pwindows <- rep(1, n)
swindows <- rep(1, n)
obs_times <- sample(8:10, n, replace = TRUE)

# Function to generate a single sample
generate_sample <- function(pwindow, swindow, obs_time) {
  rpcens(
    1, rgamma,
    shape = shape, rate = rate,
    pwindow = pwindow, swindow = swindow, D = obs_time
  )
}

# Generate samples
samples <- mapply(generate_sample, pwindows, swindows, obs_times)

# Create initial data frame
delay_data <- data.frame(
  delay = samples,
  delay_upper = samples + swindows,
  pwindow = pwindows,
  relative_obs_time = obs_times
)

head(delay_data)

# Compare the samples with and without secondary censoring to the true
# distribution
# Calculate empirical CDF
empirical_cdf <- ecdf(samples)

# Create a sequence of x values for the theoretical CDF
x_seq <- seq(0, 10, length.out = 100)

# Calculate theoretical CDF
theoretical_cdf <- pgamma(x_seq, shape = shape, rate = rate)

# Create a long format data frame for plotting
cdf_data <- data.frame(
  x = rep(x_seq, 2),
  probability = c(empirical_cdf(x_seq), theoretical_cdf),
  type = rep(c("Observed", "Theoretical"), each = length(x_seq)),
  stringsAsFactors = FALSE
)

# Plot
ggplot(cdf_data, aes(x = x, y = probability, color = type)) +
  geom_step(linewidth = 1) +
  scale_color_manual(
    values = c(Observed = "#4292C6", Theoretical = "#252525")
  ) +
  geom_vline(
    aes(xintercept = mean(samples), color = "Observed"),
    linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = shape / rate, color = "Theoretical"),
    linetype = "dashed", linewidth = 1
  ) +
  labs(
    title = "Comparison of Observed vs Theoretical CDF",
    x = "Delay",
    y = "Cumulative Probability",
    color = "CDF Type"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  coord_cartesian(xlim = c(0, 10)) # Set x-axis limit to match truncation

# Handles secondary censoring but not the primary censoring
fit <- delay_data |>
  dplyr::select(left = delay, right = delay_upper) |>
  fitdistcens(
    distr = "gamma",
    start = list(shape = 1, rate = 1)
  )

summary(fit)

# Account for prmry censoring by writing custom function using 
# primarycensoreddist
dpcens_gamma <- function(x, shape, rate) {
  result <- tryCatch(
    {
      dprimarycensored(
        x, pgamma,
        shape = shape, rate = rate,
        pwindow = 1, swindow = 1, D = 8
      )
    },
    error = function(e) {
      rep(NaN, length(x))
    }
  )
  return(result)
}

ppcens_gamma <- function(q, shape, rate) {
  result <- tryCatch(
    {
      pprimarycensored(
        q, pgamma,
        shape = shape, rate = rate,
        dpwindow = 1, D = 8
      )
    },
    error = function(e) {
      rep(NaN, length(q))
    }
  )
  return(result)
}

# Fit the model using fitdistcens with custom gamma distribution
pcens_fit <- delay_data |>
  dplyr::filter(relative_obs_time == 8) |>
  dplyr::pull(delay) |>
  fitdist(
    distr = "pcens_gamma",
    start = list(shape = 1, rate = 1)
  )

summary(pcens_fit)


