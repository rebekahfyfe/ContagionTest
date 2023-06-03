test_that("lag window works", {
  expect_equal(lag_calc(c(1, 3, 2, 7, 4, 6), 2), c(2, 2.5, 4.5, 5.5))
  expect_equal(lag_calc(c(1, 1, 1, 1, 1), 1), c(1, 1, 1, 1))
})


test_that("differencing works", {
  set.seed(3933)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x1, x2, x1, x2, x1, x2, x1)
  expect_equal(round(no_diff_data(dfS, 1), 5), list(1.00000, "results" = c(0.45719, 0.00543, 0.05901)))
})

test_that("right test", {
  set.seed(3933)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x1, x2, x1, x2, x1, x2, x1)
  expect_equal(lag_pc_test(dfS, 1, 1, F), no_diff_data(dfS, 1))
})
