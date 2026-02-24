box::use(
  .. / R / beverton_holt_model[fit_beverton_holt, estimate_theta]
)

box::use(
  stats[qnorm, pnorm, quantile, sd]
)
#' Perform Data/Pairs Bootstrap for Beverton-Holt Theta
#'
#' @description Generates a bootstrap distribution for the equilibrium parameter
#' theta by resampling the original data pairs (R_i, S_i) with replacement.
#'
#' @param data A data frame containing 'recruits' (R) and 'spawners' (S).
#' @param B An integer specifying the number of bootstrap iterations (default 1000).
#' @param seed An integer for reproducibility.
#'
#' @return A numeric vector of length B containing bootstrap estimates of theta.
#' @export
bootstrap_theta_pairs <- function(data, B = 1000, seed = 42) {
  # Set seed for reproducible research (Crucial for statistical simulations)
  set.seed(seed)

  n <- nrow(data)

  # Pre-allocate memory for performance optimization
  theta_star_1 <- numeric(B)

  for (i in 1:B) {
    # 1. Resample data indices with replacement
    boot_indices <- sample(seq_len(n), size = n, replace = TRUE)
    boot_data <- data[boot_indices, ]

    # 2. Fit the model using the OOP constructor
    # Update explicitly to match the empirical data's column names
    boot_model <- fit_beverton_holt(
      R = boot_data$recruits,
      S = boot_data$spawners
    )

    # 3. Estimate and store theta
    theta_star_1[i] <- estimate_theta(boot_model)
  }

  return(theta_star_1)
}



#' Perform Residual-based Bootstrap for Beverton-Holt Theta
#'
#' @description 
#' Generates a bootstrap distribution for the equilibrium parameter theta by 
#' resampling the residuals of the fitted linear regression model.
#' The new responses are calculated as: R_i^* = (beta_0_hat + beta_1_hat/S_i + epsilon_i^*)^(-1).
#'
#' @param data data.frame. Must contain 'recruits' and 'spawners' columns.
#' @param orig_model beverton_holt. The fitted original model object.
#' @param B numeric. Number of bootstrap iterations (default: 1000).
#' @param seed numeric. Seed for reproducibility.
#'
#' @return numeric. A vector of length B containing bootstrap estimates of theta.
#' @export
bootstrap_theta_residuals <- function(data, orig_model, B = 1000, seed = 42) {
  set.seed(seed)
  n <- nrow(data)
  theta_star_2 <- numeric(B)
  
  # 1. Extract coefficients and fitted components from the original model
  beta_0_hat <- unname(orig_model$coefficients[1])
  beta_1_hat <- unname(orig_model$coefficients[2])
  
  S_i <- data$spawners
  R_i <- data$recruits
  
  # 2. Calculate fitted values and empirical residuals
  # fitted_inv_R = beta_0_hat + beta_1_hat / S_i
  fitted_inv_R <- orig_model$model$fitted.values 
  residuals_hat <- orig_model$model$residuals
  
  # 3. Bootstrap Iterations
  for (i in 1:B) {
    # Resample residuals with replacement
    resampled_residuals <- sample(residuals_hat, size = n, replace = TRUE)
    
    # Generate bootstrap data for inverse recruits
    inv_R_star <- fitted_inv_R + resampled_residuals
    
    # Mathematical safeguard: In rare cases, resampling might produce a non-positive 
    # inverse recruit due to large variance. We filter these out to ensure R > 0.
    inv_R_star[inv_R_star <= 0] <- min(fitted_inv_R) 
    
    # Inverse the transformation to get R_i^*
    R_star <- 1 / inv_R_star
    
    # 4. Fit the model on the newly synthesized bootstrap data
    boot_model <- fit_beverton_holt(R = R_star, S = S_i)
    
    # 5. Estimate and store theta
    theta_star_2[i] <- estimate_theta(boot_model)
  }
  
  return(theta_star_2)
}

# ==============================================================================
# HELPER FUNCTIONS FOR ADVANCED CONFIDENCE INTERVALS (c, d)
# ==============================================================================

#' Compute Jackknife Estimates for Acceleration Factor (BCa)
#'
#' @description 
#' Systematically leaves out one observation at a time (Leave-One-Out) to compute
#' the jackknife distribution of theta. This is strictly required to calculate 
#' the acceleration factor (a) in the BCa method.
#'
#' @param data data.frame. The empirical dataset.
#' @return numeric. A vector of jackknife estimates.
#' @export
compute_jackknife_theta <- function(data) {
  n <- nrow(data)
  jack_ests <- numeric(n)
  
  for (i in seq_len(n)) {
    jack_data <- data[-i, ]
    model <- fit_beverton_holt(R = jack_data$recruits, S = jack_data$spawners)
    jack_ests[i] <- estimate_theta(model)
  }
  return(jack_ests)
}

#' Perform Double Bootstrap for Pairs (Studentized CI)
#'
#' @description 
#' Executes a nested resampling procedure. For each of the B outer bootstrap 
#' samples, an inner bootstrap (M iterations) is performed to empirically 
#' estimate the standard error of that specific sample.
#'
#' @param data data.frame.
#' @param B integer. Outer bootstrap iterations (default 1000).
#' @param M integer. Inner bootstrap iterations for SE estimation (default 50).
#' @param seed integer. For reproducibility.
#' @return list. Contains boot_ests (theta_star) and boot_se (SE_star).
#' @export
double_bootstrap_pairs <- function(data, B = 1000, M = 50, seed = 42) {
  set.seed(seed)
  n <- nrow(data)
  boot_ests <- numeric(B)
  boot_se <- numeric(B)
  
  for (i in seq_len(B)) {
    # 1. Outer Bootstrap Layer
    boot_indices <- sample(seq_len(n), size = n, replace = TRUE)
    boot_data <- data[boot_indices, ]
    
    outer_model <- fit_beverton_holt(R = boot_data$recruits, S = boot_data$spawners)
    boot_ests[i] <- estimate_theta(outer_model)
    
    # 2. Inner Bootstrap Layer (Estimating Standard Error)
    inner_ests <- numeric(M)
    for (j in seq_len(M)) {
      inner_indices <- sample(seq_len(n), size = n, replace = TRUE)
      inner_data <- boot_data[inner_indices, ]
      inner_model <- fit_beverton_holt(R = inner_data$recruits, S = inner_data$spawners)
      inner_ests[j] <- estimate_theta(inner_model)
    }
    boot_se[i] <- sd(inner_ests)
  }
  return(list(theta_star = boot_ests, se_star = boot_se))
}

#' Perform Double Bootstrap for Residuals (Studentized CI)
#'
#' @param data data.frame.
#' @param orig_model beverton_holt object.
#' @param B integer. Outer loops.
#' @param M integer. Inner loops.
#' @param seed integer.
#' @return list. Contains boot_ests and boot_se.
#' @export
double_bootstrap_residuals <- function(data, orig_model, B = 1000, M = 50, seed = 42) {
  set.seed(seed)
  n <- nrow(data)
  boot_ests <- numeric(B)
  boot_se <- numeric(B)
  
  fitted_inv_R <- orig_model$model$fitted.values
  residuals_hat <- orig_model$model$residuals
  S_i <- data$spawners
  
  for (i in seq_len(B)) {
    # 1. Outer Bootstrap Layer
    resampled_res <- sample(residuals_hat, size = n, replace = TRUE)
    inv_R_star <- fitted_inv_R + resampled_res
    inv_R_star[inv_R_star <= 0] <- min(fitted_inv_R) # Mathematical safeguard
    
    outer_model <- fit_beverton_holt(R = 1 / inv_R_star, S = S_i)
    boot_ests[i] <- estimate_theta(outer_model)
    
    # 2. Inner Bootstrap Layer
    outer_fitted_inv_R <- outer_model$model$fitted.values
    outer_residuals_hat <- outer_model$model$residuals
    
    inner_ests <- numeric(M)
    for (j in seq_len(M)) {
      inner_res <- sample(outer_residuals_hat, size = n, replace = TRUE)
      inv_R_star_star <- outer_fitted_inv_R + inner_res
      inv_R_star_star[inv_R_star_star <= 0] <- min(outer_fitted_inv_R)
      
      inner_model <- fit_beverton_holt(R = 1 / inv_R_star_star, S = S_i)
      inner_ests[j] <- estimate_theta(inner_model)
    }
    boot_se[i] <- sd(inner_ests)
  }
  return(list(theta_star = boot_ests, se_star = boot_se))
}