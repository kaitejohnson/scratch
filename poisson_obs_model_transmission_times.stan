functions{
  /**
  * Calculate lognormal density at a vector of indices
  *
  * @param x Vector of values at which to calculate the density
  * @param mu Mean parameter of the underlying normal distribution
  * @param sigma Standard deviation parameter of the underlying normal distribution
  * @return Vector of lognormal density values
  */
  vector lognormal_density_vector(vector x, real mu, real sigma) {
    int n = num_elements(x);
    vector[n] densities;
    
    for (i in 1:n) {
      // Use the built-in lognormal density function
      densities[i] = lognormal_lpdf(x[i] | mu, sigma);
      
      // Convert from log density to regular density
      densities[i] = exp(densities[i]);
    }
    
    return densities;
  }
  
}

data{
  int<lower=0> N_infections;
  int<lower=0> max_gi;
  array [N_infections] int inf_times;
  vector<lower=0> [max_gi] tau_vec;
 }
 
 parameters{
  real logmean_gi;
  real <lower=0>logsd_gi;

 }
 
 transformed parameters{
   vector<lower=0>[max_gi] gi;
   
   gi = lognormal_density_vector(tau_vec, logmean_gi, logsd_gi);
   
 
 }
 
 model{
   // Priors
   logmean_gi ~ normal(1, 0.5);
   logsd_gi ~ normal(0, 0.5);
 
   // Likelihood
   for(i in 1:N_infections){
     inf_times[i] ~ poisson(gi);
   }
   
 }
 