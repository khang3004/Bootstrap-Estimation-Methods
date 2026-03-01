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

# Method 1: Log-transform -> Percentile CI -> Exponential
# We calculate the CI on the log-scale first
# Then we use exp() to bring the boundaries back to days
breast_log_data <- log(data$survival[data$disease == 2])
ci_log_then_exp <- calculate_percentile_ci(breast_log_data, R = 2000)
final_ci_method1 <- exp(ci_log_then_exp)

# Method 2: Raw scale -> Percentile CI
# We calculate the CI directly on the original data
breast_raw_data <- data$survival[data$disease == 2]
final_ci_method2 <- calculate_percentile_ci(breast_raw_data, R = 2000)

# Display comparison
cat("COMPARISON OF 95% PERCENTILE CI (BREAST CANCER)\n")
cat(sprintf("Method 1: [%.2f, %.2f] \n", final_ci_method1[1], final_ci_method1[2]))
cat(sprintf("Method 2: [%.2f, %.2f] \n", final_ci_method2[1], final_ci_method2[2]))