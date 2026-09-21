#' Missing value summary
#'
#' Get summary of the missing values of data
#'
#' @param data Data
#' @returns list
get_missing_summary <- function(data) {
  out_list <- list()
  for (var in names(data)) {
    out_list[[var]] <- paste0(
      sum(is.na(data[[var]])),
      " (",
      sum(is.na(data[[var]])) / nrow(data),
      "%)"
    )
  }
  return(out_list)
}
