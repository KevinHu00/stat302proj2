test_that("mpg and wt are negatively correlated", {
  expect_true(my_lm(mpg ~ hp + wt, mtcars)[3, 1] < 0)
})
