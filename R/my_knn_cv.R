#' k-Nearest Neighbors Cross-Validation
#'
#' This function predicts output class species.
#'
#' @param train Input data frame.
#' @param cl True class value of your training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @examples
#' data(my_penguins)
#' my_knn_cv(na.omit(my_penguins)[, 3:6], na.omit(my_penguins)[, 1], 1, 5)
#' my_knn_cv(na.omit(my_penguins)[, 4:6], na.omit(my_penguins)[, 1], 2, 4)
#'
#' @return Vector of the predicted class for all observations.
#' @return Numeric with the cross-validation misclassification error.
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # split data randomly
  inds <- sample(rep(1:k_cv, length = nrow(train)), replace = TRUE)
  # combine training data frame and its true class value by column
  data <- data.frame(cbind(train, cl), "split" = inds)
  # create a vector to store predictions for all observations
  class <- rep(NA, nrow(data))
  # initialize total misclassification rate
  tot_misclas_rate <- 0
  for (i in 1:k_cv) {
    # separate training and test data
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    # predict the class value for the test data
    predictions <- as.character(class::knn(train = data_train[, 1:(ncol(data_train) - 2)],
                                    test = data_test[, 1:(ncol(data_test) - 2)],
                                    cl = data_train[, ncol(data_train) - 1],
                                    k = k_nn))
    # record predictions
    class[inds == i] <- predictions
    # sum up the misclassification rate
    tot_misclas_rate <- tot_misclas_rate +
      mean(predictions != data_test[, ncol(data_test) - 1])
  }
  # calculate average misclassification rate
  cv_err <- tot_misclas_rate / k_cv
  return(list(class, cv_err))
}
