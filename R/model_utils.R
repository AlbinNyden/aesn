#' Create model matrix for regression
#'
#' Function for creating a model matrix of the input data
#'
#' @param data Data
#' @param intercept If intercept should be included in the model
#'
#' @return matrix
create_model_matrix <- function(
    data = NULL,
    intercept = TRUE) {

  #TODO
  #Add more functionality here. Especially adding way to specify levels.

  # Create formula
  rhs <- paste(names(data), collapse = " + ")

  formula <- if (intercept) {
    stats::as.formula(paste0("~", rhs))
  } else {
    stats::as.formula(paste0("~ 0 +", rhs))
  }

  stats::model.matrix(formula, data = data)

}
