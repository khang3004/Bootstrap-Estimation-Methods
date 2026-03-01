#' Function to simulate Bootstrap for the Mean of a Cauchy Distribution
#' 
#' @param seed An integer for reproducibility.
#' @param n Sample size (number of observations)
#' @param n_boot Number of bootstrap replicates
#' @param loc Location parameter for Cauchy (default 0)
#' @param sc Scale parameter for Cauchy (default 1)
#' 
bootstrap_mean_cauchy <- function(seed, n = 100, n_boot = 2000, loc = 0, sc = 1) {

  # Generate the original "observed" data from Cauchy distribution
  # Cauchy is a heavy-tailed distribution with no finite mean/variance.
  original_data <- rcauchy(n, location = loc, scale = sc)
  
  # Compute the observed sample mean
  observed_mean <- mean(original_data)
  
  # Perform Bootstrap re sampling
  # We use replicate to run the sampling and mean calculation B times
  boot_mean <- replicate(n_boot, {
    # Re sample with replacement from the original data
    boot_sample <- sample(original_data, size = n, replace = TRUE)
    
    # Calculate the mean of this bootstrap sample
    mean(boot_sample)
  })
  
  # Return the vector of bootstrap means for further analysis
  return(boot_means)
}