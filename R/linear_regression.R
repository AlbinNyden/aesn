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

  #Create matrix model
  X <- create_model_matrix(data[,covariates],
                           intercept = intercept)
  y <- as.numeric(data[,y])

  #Solve for the coefficients
  beta <- qr.solve(X, y)

}
