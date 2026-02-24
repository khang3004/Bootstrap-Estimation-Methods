# ==============================================================================
# EXECUTION WORKFLOW
# ==============================================================================
library(tidyverse)
box::use(
  # Load modularized modeling functions
  ./R/beverton_holt_model[fit_beverton_holt, estimate_theta],
  # Load Confidence Interval (CI) formulas
  ./R/ci_functions[calculate_bca_ci, calculate_studentized_ci],
  # Load rigorous Bootstrap resampling implementations
  ./scripts/task_1_salmon[
      bootstrap_theta_pairs,
      bootstrap_theta_residuals,
      compute_jackknife_theta,
      double_bootstrap_pairs,
      double_bootstrap_residuals
  ]
)
# ==============================================================================

# 1. Load the underlying dataset
cat("Loading empirical data...\n")
salmon_data <- read.table("data/salmon.dat", header = TRUE)

# 2. Compute the initial point estimate (Task a)
# Explicitly pass the correct column names from the empirical dataframe
orig_model <- fit_beverton_holt(
  R = salmon_data$recruits,
  S = salmon_data$spawners
)

theta_hat_orig <- estimate_theta(orig_model)
cat(sprintf("Estimated Equilibrium Point (Theta_hat): %.4f\n", theta_hat_orig))

# 3. Execute Non-Parametric Resampling (Task b - Method 1: Pairs Bootstrap)
cat("Initiating Data/Pairs Bootstrap computation...\n")
theta_dist_1 <- bootstrap_theta_pairs(data = salmon_data, B = 1000)

cat("Computation completed successfully.\n")

# ==============================================================================
# PART (b): RESIDUAL BOOTSTRAP EXECUTION AND VISUALIZATION
# ==============================================================================

# 1. Execute Semi-Parametric Resampling (Residual-based Bootstrap)
cat("Initiating Residual-based Bootstrap computation...\n")
theta_dist_2 <- bootstrap_theta_residuals(
  data = salmon_data,
  orig_model = orig_model,
  B = 1000
)

# 2. Calculate Standard Error (SE) and 95% Percentile Confidence Interval
# Helper function for streamlined metric computation and aggregation
compute_metrics <- function(dist, name) {
  se <- sd(dist)
  ci <- quantile(dist, probs = c(0.025, 0.975))
  cat(sprintf("\n--- %s ---\n", name))
  cat(sprintf("Standard Error (SE): %.4f\n", se))
  cat(sprintf("95%% Percentile CI: [%.4f, %.4f]\n", ci[1], ci[2]))
  return(data.frame(Method = name, Theta_Star = dist))
}

df_dist_1 <- compute_metrics(theta_dist_1, "Pairs Bootstrap")
df_dist_2 <- compute_metrics(theta_dist_2, "Residual Bootstrap")

# 3. Aggregate empirical distributions for ggplot2 visualization
df_plot <- rbind(df_dist_1, df_dist_2)

# Professional Visualization Workflow
cat("\nGenerating comparative histograms...\n")
plot_obj <- ggplot(df_plot, aes(x = Theta_Star, fill = Method)) +
  geom_histogram(
    alpha = 0.6,
    position = "identity",
    bins = 40,
    color = "white"
  ) +
  geom_vline(
    aes(xintercept = theta_hat_orig),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c("Pairs Bootstrap" = "#00BFC4", "Residual Bootstrap" = "#F8766D")
  ) +
  labs(
    title = "Comparison of Bootstrap Distributions for Theta",
    subtitle = "Red dashed line indicates the original sample estimate",
    x = expression("Estimated Theta (" * hat(theta)^"*" * ")"),
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", legend.title = element_blank())

# Render the comparative plot
print(plot_obj)


# ==============================================================================
# PART (c): BIAS-CORRECTED AND ACCELERATED (BCa) CI
# ==============================================================================
cat("\n[+] Computing BCa Confidence Intervals...\n")

# Compute Jackknife Estimates (Required strictly once on the empirical dataset)
jack_ests <- compute_jackknife_theta(salmon_data)

# BCa CI for Pairs Bootstrap (utilizing theta_dist_1 from Task a)
bca_pairs <- calculate_bca_ci(boot_ests = theta_dist_1, orig_est = theta_hat_orig, 
                              jackknife_ests = jack_ests, alpha = 0.05)

# BCa CI for Residual Bootstrap (utilizing theta_dist_2 from Task b)
bca_resid <- calculate_bca_ci(boot_ests = theta_dist_2, orig_est = theta_hat_orig, 
                              jackknife_ests = jack_ests, alpha = 0.05)

cat(sprintf("BCa CI (Pairs): [%.4f, %.4f]\n", bca_pairs[1], bca_pairs[2]))
cat(sprintf("BCa CI (Residual): [%.4f, %.4f]\n", bca_resid[1], bca_resid[2]))

# ==============================================================================
# PART (d): STUDENTIZED (BOOTSTRAP-t) CI VIA DOUBLE BOOTSTRAP
# ==============================================================================
cat("\n[+] Initiating computationally intensive Double Bootstrap for Studentized CI...\n")
# Note: B = 1000, M = 50 => A staggering total of 50,000 models fitted per method.

# 1. Double Bootstrap Procedure for Pairs
double_pairs_out <- double_bootstrap_pairs(salmon_data, B = 1000, M = 50)
student_pairs <- calculate_studentized_ci(
  boot_ests = double_pairs_out$theta_star, 
  boot_se = double_pairs_out$se_star, 
  orig_est = theta_hat_orig, 
  orig_se = sd(double_pairs_out$theta_star), # Empirical standard error 
  alpha = 0.05
)

# 2. Double Bootstrap Procedure for Residuals
double_resid_out <- double_bootstrap_residuals(salmon_data, orig_model, B = 1000, M = 50)
student_resid <- calculate_studentized_ci(
  boot_ests = double_resid_out$theta_star, 
  boot_se = double_resid_out$se_star, 
  orig_est = theta_hat_orig, 
  orig_se = sd(double_resid_out$theta_star), 
  alpha = 0.05
)

cat(sprintf("Studentized CI (Pairs): [%.4f, %.4f]\n", student_pairs[1], student_pairs[2]))
cat(sprintf("Studentized CI (Residual): [%.4f, %.4f]\n", student_resid[1], student_resid[2]))