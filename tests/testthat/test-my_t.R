test_that("test_stat correct", {
  expect_equal(my_t.test(25:50, "two.sided", 33)[[1]],
               (mean(25:50) - 33) / (sd(25:50) / sqrt(length(25:50))))
})
test_that("df correct", {
  expect_equal(my_t.test(c(6,6,6), "less", 0)[[2]], 2)
})
test_that("test_stat correct", {
  expect_true(my_t.test(10:80, "two.sided", 45)[[4]] > 0.05)
})
test_that("test_stat correct", {
  expect_error(my_t.test(20:90, "more", 50),
               "only accept \"two.sided\", \"less\", or \"greater\"
         for the parameter `alternative`")
})



