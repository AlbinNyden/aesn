#' Normality test
#'
#' Function for normality tests
#'
#' @param x Vector in values to be used for test
#' @param tests Which tests to use
#'
#' @export
normality_test <- function(
    x = NULL,
    tests = c("shapiro_wilk", "jarque_bera")[1]) {

  #Validation
  check_numeric(x)
  check_values_in_list(values = tests,
                       check_list = c("shapiro_wilk",
                                      "jarque_bera"))

  out_list <- list()
  #Shapiro-Wilk
  if ("shapiro_wilk" %in% tests) {
    sw <- stats::shapiro.test(x)
    out_list$shapiro_wilk <- list(
      "W" = unname(sw$statistic),
      "p_value" = sw$p.value
    )
  }

  #Jarque-Bera
  if ("jarque_bera" %in% tests) {
    n <- length(x)
    m <- mean(x)

    m2 <- mean((x - m)^2)
    m3 <- mean((x - m)^3)
    m4 <- mean((x - m)^4)

    skewness <- m3 / m2^(3 / 2)
    kurtosis <- m4 / m2^2

    JB <- n / 6 *
      (skewness^2 + (kurtosis - 3)^2 / 4)

    p <- pchisq(
      JB,
      df = 2,
      lower.tail = FALSE
    )

    out_list$jarque_bera <- list(
      "JB" = JB,
      "p_value" = p
    )
  }

  return(out_list)

}
