#' Bootstrap statistic function
#' @param data: The input vector (numeric).
#' @param indices: The indices provided by the boot() function for resampling.
#' @return A vector containing [mean, variance of the mean].
#' @export
calculate_boot_stats <- function(data, indices) {
  # Create the bootstrap resample
  resample <- data[indices]
  
  # Calculate the primary statistic (mean)
  m <- mean(resample)
  
  # Calculate the variance of the mean (required for studentized CI)
  # formula: Var(mean) = Var(sample) / n
  v <- var(resample) / length(resample)
  
  return(c(m, v))
}

#' Formatted Output for Bootstrap Confidence Intervals
#' @param boot_obj An object of class 'boot'.
#' @param label String describing the group (e.g., "Stomach Cancer").
#' @param conf_level Numeric confidence level (default is 0.95).
print_boot_results <- function(boot_obj, label, conf_level = 0.95) {
  
  # Calculate CI for both Studentized and BCa types
  ci_res <- boot::boot.ci(boot_obj, conf = conf_level, type = c("stud", "bca"))
  
  cat(" RESULTS FOR:", toupper(label), "\n")
  
  # Print Studentized CI (extracted from index 4 and 5 of the 'stud' element)
  cat(sprintf(" 95%% Studentized CI: [%.4f, %.4f]\n", exp(ci_res$stud[4]), exp(ci_res$stud[5])))
  
  # Print BCa CI (extracted from index 4 and 5 of the 'bca' element)
  cat(sprintf(" 95%% BCa CI        : [%.4f, %.4f]\n", exp(ci_res$bca[4]), exp(ci_res$bca[5])))
}

#' Permutation Test
#' @param group1 Vector of dataset in group 1 (log scale)
#' @param group2 Vector of dataset in group 2 (log scale)
#' @param n_perm number of permitations
#' @return List of observed differents, p_value, permutation differents
#' @export
run_permutation_test <- function(group1, group2, n_perm = 9999) {
  
  # Calculate the actual observed difference in means
  obs_diff <- mean(group1) - mean(group2)
  
  # Combine data into one pool to represent the null hypothesis (H0)
  # H0 assumes there is no group effect, so labels are interchangeable
  combined_data <- c(group1, group2)
  n1 <- length(group1)
  n_total <- length(combined_data)
  
  # Pre-allocate vector to store results of each permutation
  perm_diffs <- numeric(n_perm)
  
  # Set seed for reproducibility within the function
  set.seed(123)
  
  for (i in 1:n_perm) {
    # Randomly shuffle indices without replacement
    shuffled_indices <- sample(1:n_total, n1, replace = FALSE)
    
    # Split the shuffled data into two new temporary groups
    perm_group1 <- combined_data[shuffled_indices]
    perm_group2 <- combined_data[-shuffled_indices]
    
    # Calculate the mean difference for this specific permutation
    perm_diffs[i] <- mean(perm_group1) - mean(perm_group2)
  }
  
  # Calculate Two-Tailed P-value: The proportion of permuted differences 
  # as extreme as or more extreme than the observed one
  p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
  
  # Return result
  return(list(observed_diff = obs_diff, p_value = p_value, perm_dist = perm_diffs))
}

#' Calculate Percentile Bootstrap Confidence Interval
#' @param data Vector of data points.
#' @param R Number of bootstrap replicates.
#' @param is_log Boolean, TRUE if data is already log-transformed.
#' @return vector of results
#' @export
calculate_percentile_ci <- function(data, R = 2000) {
  # We only need the mean for the percentile method
  boot_out <- boot::boot(data, function(d, i) mean(d[i]), R = R)
  # Type "perc" refers to the Percentile Method
  ci <- boot::boot.ci(boot_out, type = "perc")
  # Return only the lower and upper bounds [index 4 and 5]
  return(ci$perc[4:5])
}