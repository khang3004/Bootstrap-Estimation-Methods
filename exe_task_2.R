# Load libraries and source dataset
library(boot)
data <- read.table(here::here('data', 'cancersurvival.dat'), header = TRUE)

# Filter groups and apply log-transformation
stomach <- log(data$survival[data$disease == 1])
breast  <- log(data$survival[data$disease == 2])

# Set parameters
set.seed(123)
iterations <- 2000

# Perform bootstrap for stomach cancer and breast cancer with bootstrap studentized and BCa
boot_stomach <- boot(data = stomach, statistic = calculate_boot_stats, R = iterations)
boot_breast <- boot(data = breast, statistic = calculate_boot_stats, R = iterations)

# Display average survival time for stomach cancer and breast cancer
print_boot_results(boot_stomach, "Stomach Cancer (Group 1)")
print_boot_results(boot_breast, "Breast Cancer (Group 2)")

# Run permutation test to compare stomach and breast cancer survival on log-scale
perm_results <- run_permutation_test(stomach, breast, n_perm = 9999)

# Display result summary
cat("HYPOTHESIS TEST: PERMUTATION TEST\n")
cat(sprintf(" Observed mean difference: %.4f\n", perm_results$observed_diff))
cat(sprintf(" Empirical P-value       : %.4f\n", perm_results$p_value))

# We compare our calculated P-value against the significance level (alpha)
# If p < 0.05, the observed difference is unlikely to occur by chance
# If p >= 0.05, the difference could easily be due to random variation
alpha <- 0.05
if (perm_results$p_value < alpha) {
  cat(" Result: statistically significant \n")
} else {
  cat(" Result: Not statistically significant \n")
}

# This plot shows the distribution of differences if null hypothesis (H0) were true
hist(perm_results$perm_dist, 
     breaks = 50, 
     main = "Permutation distribution (Null hypothesis)",
     xlab = "Differences calculated from permutations", 
     col = "lightgrey", border = "white")

# Add a vertical line for the observed difference
abline(v = perm_results$observed_diff, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Observed difference", col = "red", lty = 2, lwd = 2)

