#' Random Forest Cross-Validation
#'
#' This function predicts output body_mass_g.
#'
#' @param k Number of folds.
#' @keywords prediction
#'
#' @return Numeric with the cross-validation error.
#'
#' @examples
#' data(my_penguins)
#' my_rf_cv(3)
#' my_rf_cv(4)
#'
#' @export
my_rf_cv <- function(k) {
  # store penguins data
  penguins_df <- my_penguins
  # omit rows with NA entry
  penguins_df <- na.omit(penguins_df)
  # split data randomly
  inds <- sample(rep(1:k, length = nrow(penguins_df)))
  # combine training data frame and its true numeric value by column
  data <- data.frame(cbind(penguins_df[, 3:5], penguins_df[, 6]), "split" = inds)
  # initialize total MSE
  tot_MSE <- 0
  for (i in 1:k) {
    # separate training and test data
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    # train a random forest model with 100 trees
    model_rf <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                               flipper_length_mm, data = data_train[, 1:(ncol(data_train) - 1)], ntree = 100)
    # predict the body_mass_g using the previous model
    predictions <- predict(model_rf, data_test[, 1:(ncol(data_test) - 2)])
    # sum up the MSE
    tot_MSE <- tot_MSE +
      (sum((data_test[, ncol(data_test) - 1] - predictions)^2) / length(predictions))
  }
  # return the average MSE across all k folds
  return(tot_MSE / k)
}
