# Simulate Bootstrap for the Mean of a Cauchy Distribution
# Set seed, sample size, number of bootstrap replicates, Cauchy distribution parameters
seed <- set.seed(123)
n <- 100         
n_boot <- 2000
loc <- 0
sc <- 1

# Generate the original "observed" data from Cauchy distribution
# Cauchy is a heavy-tailed distribution with no finite mean/variance.
original_data <- rcauchy(n, location = loc, scale = sc)

# Compute the observed sample mean
observed_mean <- mean(original_data)

# Compute boostrap for the mean of Cauchy distribution
boot_means <- bootstrap_mean_cauchy(seed, n, n_boot, loc, sc)

# Visualization to show the failure
# If bootstrap worked, this would look like a stable Normal curve.
# For Cauchy, it will look erratic with extreme outliers.
hist(boot_means, 
     breaks = 50, 
     main = "Bootstrap Distribution (Cauchy Mean)",
     xlab = "Bootstrap Means", 
     col = "skyblue", 
     border = "white"
)

# Add a vertical line for the mean of the original sample
abline(v = observed_mean, col = "red", lwd = 2)

# Simulate Bootstrap for the maximum of U(0, theta)
# Set seed, sample size, number of bootstrap replicates, uniform distribution parameters
seed <- set.seed(123)
n <- 50
n_boot <- 2000
theta <- 10

# 1. Generate original data
original_data <- runif(n, min = 0, max = theta)
theta_hat <- max(original_data)

# Compute bootstrap for the max of uniform distribution
boot_maxes <- boostrap_max_uniform(seed, n, n_boot, theta)

# Visualization to show the failure
hist(boot_maxes, 
     breaks = 30, 
     col = "orange",
     main = "Bootstrap Failure: Uniform Max",
     xlab = "Bootstrap Max Values",
     border = "white"
     )

# Add a red line at the sample maximum to show the "wall"
abline(v = theta_hat, col = "red", lwd = 2, lty = 2)

