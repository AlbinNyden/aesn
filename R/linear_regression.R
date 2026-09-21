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

  #Handle missing values
  missing_value_summary <- get_missing_summary(data[c(y, covariates)])
  n_row_orig <- nrow(data)
  data <- na.omit(data[c(y, covariates)])

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
  rss_mean <- mean(res^2)

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
  p_value <- 2 * stats::pt(
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

  # Normality of residuals
  res_norm_test <- normality_test(
    res,
    tests = c("shapiro_wilk", "jarque_bera")
  )

  #Output
  model <-list(
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
      "residual_sum_of_squares" = rss,
      "mean_residual_sum_of_squares" = rss_mean
    ),
    "normality_of_residuals" = res_norm_test,
    "data_quality" = list(
      "missing_value_summary" = missing_value_summary,
      "original_size" = n_row_orig,
      "rows_removed" = n_row_orig - n
    )
  )

  class(model) <- "aesn_linear_regression"

  model

}

#' Print summary for Linear regression
#'
#' Function printing the summary of the fitted model
#'
#' @param object aesn_linear_regression
#'
#' @export
print.aesn_linear_regression <- function(model, ...) {

  #Prettify print data frame
  df_print <- model$coefficients$data_frame
  df_print$p_value <- ifelse(
    df_print$p_value <= 0.05,
    paste0(df_print$p_value, "*"),
    paste0(df_print$p_value)
  )
  colnames(df_print) <- c("Variable", "Coefficient",
                          "t value", "std. error", "p-value")

  cat("Linear Regression\n")
  cat("===================\n\n")
  print.data.frame(df_print, row.names = FALSE)
  cat("\n")
  cat("(* implies a p-value less than 0.05)\n")
  cat("===== Goodness of fit ===== \n\n")
  cat(paste0("R squared: ",
             round(model$statistics$r_squared, 3),
             "\n"))
  cat(paste0("Adjusted R squared: ",
             round(model$statistics$adjusted_r_squared, 3),
             "\n"))
  cat(paste0("Mean of sum of square of residuals: ",
             round(model$statistics$mean_residual_sum_of_squares, 3),
             "\n"))
  cat("\n")
  cat("===== Model assumptions =====\n")
  cat("--- Normality of residuals ---\n")
  cat(paste0("Shapiro-Wilk: ",
             "W = ",
             round(model$normality_of_residuals$shapiro_wilk$W, 3),
             ", p-value = ",
             model$normality_of_residuals$shapiro_wilk$p_value))
  cat("\n\n")
  cat("===== Data quality =====\n")
  cat(paste0(model$data$rows_removed,
             " (",
             model$data$rows_removed / model$data$original_size,
             "%) rows removed due to missing values"))
  cat("\n")
  cat("--- Missing values ---\n")
  print.data.frame(as.data.frame(model$data$missing_value_summary),
                   row.names = FALSE)
}

