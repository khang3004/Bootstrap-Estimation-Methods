#' Function to simulate Bootstrap for the Mean of a Cauchy Distribution
#' 
#' @param seed An integer for reproducibility.
#' @param n Sample size (number of observations)
#' @param n_boot Number of bootstrap replicates
#' @param loc Location parameter for Cauchy (default 0)
#' @param sc Scale parameter for Cauchy (default 1)
#' 
bootstrap_mean_cauchy <- function(seed, n = 100, n_boot = 2000, loc = 0, sc = 1) {
  # Set seed for reproducibility of the simulation
  set.seed(seed)

  # Generate the original "observed" data from Cauchy distribution
  # Cauchy is a heavy-tailed distribution with no finite mean/variance.
  original_data <- rcauchy(n, location = loc, scale = sc)
  
  # Compute the observed sample mean
  observed_mean <- mean(original_data)
  
  # Perform Bootstrap resampling
  # We use replicate to run the sampling and mean calculation n_boot times
  boot_means <- replicate(n_boot, {
    # Re sample with replacement from the original data
    boot_sample <- sample(original_data, size = n, replace = TRUE)
    
    # Calculate the mean of this bootstrap sample
    mean(boot_sample)
  })
  
  # Return the vector of bootstrap means for further analysis
  return(boot_means)
}

#' Function to simulate Bootstrap for the maximum of U(0, theta)
#' 
#' @param seed An integer for reproducibility.
#' @param n: Sample size
#' @param n_boot: Number of bootstrap replicates
#' @param theta: The true upper bound of the Uniform distribution
#' 
boostrap_max_uniform <- function(seed, n, n_boot, theta) {
  # Set seed for reproducibility of the simulation
  set.seed(seed)
  
  # Generate the initial "observed" dataset from U(0, theta)
  original_data <- runif(n, min = 0, max = theta)
  
  # The Maximum Likelihood Estimator (MLE) for theta is the sample maximum
  theta_hat_obs <- max(original_data)
  
  # Perform the Bootstrap procedure
  boot_maxes <- replicate(n_boot, {
    # Resample with replacement: some values will be repeated, some omitted
    boot_sample <- sample(original_data, size = n, replace = TRUE)
    
    # Calculate the statistic of interest (the maximum) for the bootstrap sample
    max(boot_sample)
  })
  
  # Return a list containing the bootstrap distribution and the observed statistic
  return(boot_maxes)
}