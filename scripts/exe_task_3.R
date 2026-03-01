#
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

boot_means <- bootstrap_mean_cauchy(seed, n, n_boot, loc, sc)

# Visualization to show the failure
# If bootstrap worked, this would look like a stable Normal curve.
# For Cauchy, it will look erratic with extreme outliers.
hist(boot_means, 
     breaks = 50, 
     main = "Bootstrap Distribution (Cauchy Mean)",
     xlab = "Bootstrap Means", col = "skyblue", border = "white")

# Add a vertical line for the mean of the original sample
abline(v = observed_mean, col = "red", lwd = 2)
