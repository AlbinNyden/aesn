#' Test data
#'
#' Creates test data
create_test_data <- function() {

  set.seed(1234)
  df_test <- data.frame(
    age = floor(stats::runif(1000, min = 18, max = 100)),
    height = stats::rnorm(1000, mean = 175, sd = 7),
    weight = stats::rnorm(1000, mean = 80, sd = 5)
  )
  df_test$bmi <- df_test$weight / (df_test$height / 100)^2
  df_test$heart_disease <- ifelse(
    df_test$age >= 60,
    stats::rbinom(n = 1, size = 1, prob = 0.8),
    stats::rbinom(n = 1, size = 1, prob = 0.5)
  )

  #Save data
  usethis::use_data(df_test, overwrite = TRUE)

}
