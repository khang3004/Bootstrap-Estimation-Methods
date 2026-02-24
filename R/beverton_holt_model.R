# Explicitly import necessary functions from the 'stats' package
# This ensures module isolation and prevents namespace pollution
box::use(
  stats[lm, coef]
)
#' Fit a Beverton-Holt Model using Linear Transformation
#'
#' @description This constructor function fits a Beverton-Holt model by
#' transforming the non-linear relationship R = 1 / (beta_0 + beta_1/S)
#' into a simple linear regression: 1/R = beta_0 + beta_1 * (1/S).
#'
#' @param R A numeric vector of recruits.
#' @param S A numeric vector of spawners.
#'
#' @return An S3 object of class 'beverton_holt' containing the linear model,
#' original data, and fitted parameters.
#' @export
fit_beverton_holt <- function(R, S) {
  # 1. Input validation
  if (!is.numeric(R) || !is.numeric(S)) {
    stop("Inputs must be numeric vectors.")
  }
  if (length(R) != length(S)) {
    stop("Vectors R and S must have equal length.")
  }
  if (any(R <= 0) || any(S <= 0)) {
    stop("R and S must be strictly positive.")
  }

  # 2. Linear Transformation
  inv_R <- 1 / R
  inv_S <- 1/S

  # 3. Fit the OLS (Ordinary least squares) model
  model <- lm(inv_R ~ inv_S)

  # 4. Construct the S3 object
  obj <- list(
    model=model,
    data=data.frame(R=R, S=S),
    coefficients=coef(model)
  )
  # Assign class for S3 Object-Oriented Programming compatibility
  class(obj) <- "beverton_holt"
  return(obj)
}

#' Estimate the Stable Population Level (Theta)
#'
#' @description Calculates the equilibrium point where R = S.
#' Mathematically derived as: theta = (1 - beta_1) / beta_0.
#'
#' @param obj An object of class 'beverton_holt'.
#'
#' @return A single numeric value representing the estimated theta.
#' @export

estimate_theta <- function(obj) {
  if (!inherits(obj, "beverton_holt")) stop("Object must be of class 'beverton_holt'.")
  
  beta_0 <- unname(obj$coefficients[1])
  beta_1 <- unname(obj$coefficients[2])
  
  # Calculate theta
  theta_hat <- (1 - beta_1) / beta_0
  return(theta_hat)
}