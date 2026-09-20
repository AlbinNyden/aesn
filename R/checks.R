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
      paste0("Follwing variables not found in data: \n",
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

#' Check if values are in list
#'
#' @param values Values to check
#' @param check_list Values to check 'values' against
check_values_in_list <- function(
    values,
    check_list) {

  non_in_check_list <- values[!(values %in% check_list)]
  if (length(non_in_check_list) > 0) {
    stop(
      paste0("The following values: \n",
             paste0(non_in_check_list, collapse = "\n"),
             "\nare not in\n",
             paste0(check_list, collapse = "\n")),
      call. = FALSE
    )
  }

}
