#' t-test
#'
#' This function conducts a one sample t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative Character string specifying the alternative hypothesis.
#' @param mu Numeric indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List containing the numeric test statistic, the degree of freedom,
#'   the value of the alternative, the numeric p-value.
#'
#' @examples
#' my_t.test(c(4,5,6,7,8,9), "less", 3)
#' my_t.test(c(11,13,17,20,33), "two.sided", 15)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # check whether the input for alternative parameter is acceptable
  # give informative error message if not
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("only accept \"two.sided\", \"less\", or \"greater\"
         for the parameter `alternative`")
  }
  # calculate test statistic
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  # calculate degree of freedom
  df = length(x) - 1
  # calculate p-value according to alternative
  if (alternative == "two.sided") {
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  }
  # return a list with test results
  return(list("test_stat" = test_stat,
              "df" = df,
              "alternative" = alternative,
              "p_val" = p_val))
}
