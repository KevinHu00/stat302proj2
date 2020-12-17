#' Linear Model
#'
#' This function fits a linear model.
#'
#' @param formula Formula class object.
#' @param data Input data frame.
#' @keywords prediction
#'
#' @return Coefficient table.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#' my_lm(mpg ~ hp + wt + drat, mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # extract the model matrix X
  X <- model.matrix(formula, data)
  # extract the model response Y
  Y <- model.response(model.frame(formula, data))
  # solve for linear regression coefficients
  estimate <- solve(t(X) %*% X) %*% t(X) %*% Y
  # create a vector with length equal to the number of
  # observations in the data
  temp <- rep(0, nrow(data))
  # calculate and store the sigma squared for each observation
  for (i in 1:nrow(data)) {
    temp[i] <- (((Y[i] - X[i, ] %*% estimate)) ^ 2) /
      (nrow(data) - length(estimate))
  }
  # sump up the sigma squared for each observation
  sigma_2 <- sum(temp)
  # calculate the standard error for each coefficient
  std_error <- sqrt(sigma_2 * diag(solve(t(X) %*% X)))
  # calculate the t value for each coefficient
  t_val <- estimate / std_error
  # calculate the p value for each coefficient
  pr <- 2 * pt(abs(t_val),
               nrow(data) - length(estimate),
               lower.tail = FALSE)
  # combine columns of the coefficient to form a summary table
  lm_summary <- cbind(estimate, std_error, t_val, pr)
  # assign column names to the table
  colnames(lm_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(lm_summary)
}
