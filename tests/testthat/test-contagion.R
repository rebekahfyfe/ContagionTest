test_that("lag window works", {
  expect_equal(lag_calc(c(1, 3, 2, 7, 4, 6), 2), c(2, 2.5, 4.5, 5.5))
  expect_equal(lag_calc(c(1, 1, 1, 1, 1), 1), c(1, 1, 1, 1))
})


test_that("stationary data", {
  set.seed(3933)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x1, x2, x1, x2, x1, x2, x1)
  expect_equal(round(unname(no_diff_data(dfS, 1)[-1]), 5), c(0.47815, -0.00758, 0.04599))
})

test_that("non stationary data", {
  set.seed(3933)
  x1 <- runif(100, 0, 1) * seq(1:100)
  x2 <- runif(100, 0, 1) * seq(1:100)
  x3 <- runif(100, 0, 1) * seq(1:100)
  x4 <- runif(100, 0, 1) * seq(1:100)
  x5 <- runif(100, 0, 1) * seq(1:100)
  x6 <- runif(100, 0, 1) * seq(1:100)
  x7 <- runif(100, 0, 1) * seq(1:100)
  x8 <- runif(100, 0, 1) * seq(1:100)
  dfS <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8)
  lag_pc_test(dfS, 10, 1, T)
  expect_equal(round(unname(diff_data(dfS, 1)[-1]), 5), c(0.47815, -0.00758, 0.04599))
})

test_that("right test", {
  set.seed(3933)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x1, x2, x1, x2, x1, x2, x1)
  #set.seed(3933)
  nodif <- round(no_diff_data(dfS, 1)[-1], 3)
  #nodif
  set.seed(3933)
  expect_equal(round(lag_pc_test(dfS, 1, 1, F, 0.1, 1, F), 3), nodif)
})
