#' Linear regression
#'
#' Function for computing a standard linear regression
#'
#' @param data Data
#' @param y Name of y variable to predict
#' @param covariates Variables to include in the model
#' @param intercept Whether to include intercept in the model
#'
#' @returns list
#' @export
linear_regression <- function(
    data = NULL,
    y = NULL,
    covariates = NULL,
    intercept = TRUE) {

  #Validation
  check_vars_exist(data = data, vars = unique(c(y, covariates)))
  check_numeric(data[,y])

  #TODO handle missing values

  #Create matrix model
  X <- create_model_matrix(data[covariates],
                           intercept = intercept)
  y <- as.numeric(data[,y])

  #Solve for the coefficients
  beta <- qr.solve(X, y)

  beta_df <- data.frame(
    "variable" = names(beta),
    "coefficient" = unname(beta)
  )

  #Predictions
  fitted_values <- as.vector(X %*% beta)

  #Residuals
  res <- y - fitted_values

  # Degrees of freedom
  n <- nrow(X)
  p <- ncol(X)
  df_residual <- n - p

  # RSS
  rss <- sum(res^2)

  # Residual variance
  sigma2 <- rss / df_residual

  # Covariance matrix
  XtX_inv <- solve(crossprod(X))

  vcov <- sigma2 * XtX_inv

  # Standard errors
  std_error <- sqrt(diag(vcov))

  # t statistics
  t_value <- beta / std_error
  beta_df$t_value <- unname(t_value)
  beta_df$std_error <- unname(std_error)

  # p-values
  p_value <- 2 * pt(
    abs(t_value),
    df = df_residual,
    lower.tail = FALSE
  )
  beta_df$p_value <- unname(p_value)

  # R squared
  tss <- sum((y - mean(y))^2)

  r_squared <- 1 - rss / tss

  adjusted_r_squared <-
    1 - (1 - r_squared) *
    (n - 1) / df_residual

  #Output
  list(
    "model" = "linear_regression",
    "coefficients" = list(
      "vector" = beta,
      "data_frame" = beta_df
    ),
    "residuals" = res,
    "fitted_values" = fitted_values,
    "statistics" = list(
      "r_squared" = r_squared,
      "adjusted_r_squared" = adjusted_r_squared,
      "residual_sum_of_squares" = rss
    )
  )

}
