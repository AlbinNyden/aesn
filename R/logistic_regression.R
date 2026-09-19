#' Logistic regression
#'
#' Creates a logistic regression model
#'
#' @param data Data
#' @param y The binary value to estimate
#' @param covariates The variables used as covariates
#' @param intercept If intercept should be included
#' @param max_iter Maximum number of iterations
#' @param tolerance Tolerance of difference in estimation of coefficients
#'
#' @returns list Contains the model
#' @export
logistic_regression <- function(
    data = NULL,
    y = NULL,
    covariates = NULL,
    intercept = TRUE,
    max_iter = 100,
    tol = 1e-6) {

  #Validation
  check_vars_exist(data = data, vars = unique(c(y, covariates)))
  check_numeric(data[,y])

  #Create matrix model
  X <- create_model_matrix(data[covariates],
                           intercept = intercept)
  y <- as.numeric(data[,y])

  #Estimation
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)

  for (iteration in seq_len(max_iter)) {

    # Linear predictor
    eta <- as.vector(X %*% beta)

    # Predicted probabilities
    prob <- 1 / (1 + exp(-eta))

    # Avoid probabilities exactly equal to 0 or 1
    prob <- pmin(pmax(prob, 1e-15), 1 - 1e-15)

    # -----------------------------
    # Gradient
    # -----------------------------

    gradient <- crossprod(X, y - prob)

    # -----------------------------
    # Weight matrix
    # -----------------------------

    w <- prob * (1 - prob)

    # X'WX
    XWX <- crossprod(X, X * w)

    # -----------------------------
    # Newton-Raphson step
    # -----------------------------

    step <- solve(XWX, gradient)

    beta_new <- beta + step

    # -----------------------------
    # Convergence
    # -----------------------------

    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      converged <- TRUE
      break
    }

    beta <- beta_new
    converged <- FALSE
  }

  beta
}
