test_that("the more the fold, the lower the CV MSE", {
  expect_true(my_rf_cv(2) > my_rf_cv(10))
})
