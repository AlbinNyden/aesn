#' Columns exist validation
#'
#' Checks if the columns in data exist.
#'
#' @param data Data
#' @param vars Variables to check if they exist in Data
#'
check_vars_exist <- function(
    data = NULL,
    vars = NULL) {

  if (is.null(data)) {
    stop(
      sprintf("Input data cannot be NULL"),
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop(
      sprintf("`data` must be a data.frame."),
      call. = FALSE)
  }


  if (!(all(vars %in% names(data)))) {
    missing_vars <- vars[!vars %in% names(data)]
    missing_vars <- paste0(missing_vars, "\n")
    stop(
      paste0("Follwing variables not found in data: ",
             missing_vars),
      call. = FALSE
    )
  }

}

#' Check if value is numeric
#'
#' @param value Value to check if it is numeric
check_numeric <- function(
    value) {
  value_name <- deparse(substitute(value))
  if (!is.numeric(value)) {
    stop(sprintf("'%s' must be numeric.", value_name),
         call. = FALSE)
  }
  invisible(value_name)
}
