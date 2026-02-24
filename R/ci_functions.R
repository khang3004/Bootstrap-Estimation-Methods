# Explicitly import vital statistical functions to prevent namespace pollution
box::use(
  stats[qnorm, pnorm, quantile, sd]
)
#' Calculate Bias-Corrected and Accelerated (BCa) Confidence Interval
#'
#' @description Computes the BCa interval for a given set of bootstrap estimates.
#' It requires the original estimate and a function to compute jackknife values
#' to derive the acceleration factor (a).
#'
#' @param boot_ests A numeric vector of bootstrap estimates (theta_star).
#' @param orig_est The original sample estimate (theta_hat).
#' @param jackknife_ests A numeric vector of jackknife estimates.
#' @param alpha The significance level (default is 0.05).
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds.
#' @export
calculate_bca_ci <- function(boot_ests, orig_est, jackknife_ests, alpha = 0.05) {
  B <- length(boot_ests)
  
  # 1. Bias-correction factor (z0)
  p_less <- sum(boot_ests < orig_est) / B
  z0 <- qnorm(p_less)
  
  # 2. Acceleration factor (a) using Jackknife
  theta_dot <- mean(jackknife_ests)
  diffs <- theta_dot - jackknife_ests
  numerator <- sum(diffs^3)
  denominator <- 6 * (sum(diffs^2))^(3/2)
  a <- numerator / denominator
  
  # 3. Calculate adjusted percentiles
  z_alpha <- qnorm(alpha / 2)
  z_1_alpha <- qnorm(1 - alpha / 2)
  
  alpha_1 <- pnorm(z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha)))
  alpha_2 <- pnorm(z0 + (z0 + z_1_alpha) / (1 - a * (z0 + z_1_alpha)))
  
  # 4. Extract quantiles
  ci_lower <- quantile(boot_ests, probs = alpha_1, names = FALSE)
  ci_upper <- quantile(boot_ests, probs = alpha_2, names = FALSE)
  
  return(c(Lower = ci_lower, Upper = ci_upper))
}

#' Calculate Studentized (Bootstrap-t) Confidence Interval
#'
#' @description Computes the Studentized CI. This method requires a standard 
#' error estimate for EACH bootstrap sample (often requiring double bootstrap).
#'
#' @param boot_ests A numeric vector of bootstrap estimates (theta_star).
#' @param boot_se A numeric vector of standard errors for each boot_est.
#' @param orig_est The original sample estimate (theta_hat).
#' @param orig_se The standard error of the original estimate.
#' @param alpha The significance level (default is 0.05).
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds.
#' @export
calculate_studentized_ci <- function(boot_ests, boot_se, orig_est, orig_se, alpha = 0.05) {
  
  # 1. Calculate the bootstrap t-statistics (Z_star)
  z_star <- (boot_ests - orig_est) / boot_se
  
  # 2. Find the quantiles of the Z_star distribution
  t_lower <- quantile(z_star, probs = alpha / 2, names = FALSE)
  t_upper <- quantile(z_star, probs = 1 - alpha / 2, names = FALSE)
  
  # 3. Construct the interval (Note the inversion of bounds)
  ci_lower <- orig_est - t_upper * orig_se
  ci_upper <- orig_est - t_lower * orig_se
  
  return(c(Lower = ci_lower, Upper = ci_upper))
}