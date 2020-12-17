test_that("the more the fold, the lower the CV MSE", {
  expect_true(my_knn_cv(na.omit(my_penguins)[, 3:6], na.omit(my_penguins)[, 1], 4, 2)[[2]] >
                my_knn_cv(na.omit(my_penguins)[, 3:6], na.omit(my_penguins)[, 1], 4, 20)[[2]])
})
