test_that("lag window works", {
  expect_equal(lag_calc(c(1, 3, 2, 7, 4, 6), 2), c(2, 2.5, 4.5, 5.5))
  expect_equal(lag_calc(c(1, 1, 1, 1, 1), 1), c(1, 1, 1, 1))
})


test_that("stationary data", {
  set.seed(39)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x1, x2, x1, x2, x1, x2, x1)
  expect_equal(round(unname(no_diff_data(dfS, 1)[-1]), 2), c(0.46, 0.01, 0.01))
})

test_that("non stationary data", {
  set.seed(39)
  x1 <- runif(100, 0, 1) * seq(1:100)
  x2 <- runif(100, 0, 1) * seq(1:100)
  x3 <- runif(100, 0, 1) * seq(1:100)
  x4 <- runif(100, 0, 1) * seq(1:100)
  x5 <- runif(100, 0, 1) * seq(1:100)
  x6 <- runif(100, 0, 1) * seq(1:100)
  x7 <- runif(100, 0, 1) * seq(1:100)
  x8 <- runif(100, 0, 1) * seq(1:100)
  dfS <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8)
  expect_equal(round(unname(diff_data(dfS, 1)[-1]), 2), c(0.93, -0.40, 0.04))
})

test_that("right test stationary", {
  set.seed(39)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  x3 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x3, x1, x2, x3, x1, x2, x3, x1, x2, x3)
  set.seed(39)
  nodif <- round(no_diff_data(dfS, 1)[-1], 1)
  set.seed(39)
  main <- round(lag_pc_test(dfS, 1, 1, F, 0.1, 1, F), 1)
  expect_equal(main, nodif)
})

test_that("right test non stationary", {
  set.seed(39)
  x1 <- runif(100, 0, 1) * seq(1:100)
  x2 <- runif(100, 0, 1) * seq(1:100)
  x3 <- runif(100, 0, 1) * seq(1:100)
  x4 <- runif(100, 0, 1) * seq(1:100)
  x5 <- runif(100, 0, 1) * seq(1:100)
  x6 <- runif(100, 0, 1) * seq(1:100)
  x7 <- runif(100, 0, 1) * seq(1:100)
  x8 <- runif(100, 0, 1) * seq(1:100)
  x9 <- runif(100, 0, 1) * seq(1:100)
  x10 <- runif(100, 0, 1) * seq(1:100)
  dfS <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  set.seed(39)
  main <- round(lag_pc_test(dfS, 1, 1, T, unitTest = T)[1], 2)
  set.seed(39)
  diff <- round(diff_data(dfS, 1)[1], 2)
  expect_equal(main, diff)
})

test_that("missing data parameter works stationary", {
  set.seed(39)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  ind1 <- sample(100, 50)
  x1[ind1] <- NA
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  ind2 <- sample(100, 50)
  x2[ind2] <- NA
  x3 <- rbinom(n = 100, size = 1, prob = 0.5)
  x4 <- rbinom(n = 100, size = 1, prob = 0.5)
  x5 <- rbinom(n = 100, size = 1, prob = 0.5)
  dfS <- data.frame(x1, x2, x3, x4, x5)
  main <- lag_pc_test(dfS, 1, 1, T, 0.1, 1, T, T)
  expect_equal(main[[1]], 0)
})


test_that("missing data parameter works nonstationary", {
  set.seed(39)
  x6 <- runif(100, 0, 1) * seq(1:100)
  ind6 <- sample(100, 50)
  x6[ind6] <- NA
  x7 <- runif(100, 0, 1) * seq(1:100)
  ind7 <- sample(100, 50)
  x7[ind7] <- NA
  x8 <- runif(100, 0, 1) * seq(1:100)
  ind8 <- sample(100, 15)
  x8[ind8] <- NA
  x9 <- runif(100, 0, 1) * seq(1:100)
  x10 <- runif(100, 0, 1) * seq(1:100)
  dfS <- data.frame(x6, x7, x8, x9, x10)
  main <- lag_pc_test(dfS, 1, 1, T, 0.1, 1, T, T)
  expect_equal(main[[1]], 1)
})

test_that("missing data parameter works", {
  set.seed(39)
  x1 <- rbinom(n = 100, size = 1, prob = 0.5)
  ind1 <- sample(100, 15)
  x1[ind1] <- NA
  x2 <- rbinom(n = 100, size = 1, prob = 0.5)
  ind2 <- sample(100, 15)
  x2[ind2] <- NA
  x3 <- rbinom(n = 100, size = 1, prob = 0.5)
  x4 <- rbinom(n = 100, size = 1, prob = 0.5)
  x5 <- rbinom(n = 100, size = 1, prob = 0.5)
  x6 <- runif(100, 0, 1) * seq(1:100)
  ind6 <- sample(100, 15)
  x6[ind6] <- NA
  x7 <- runif(100, 0, 1) * seq(1:100)
  x8 <- runif(100, 0, 1) * seq(1:100)
  x9 <- runif(100, 0, 1) * seq(1:100)
  x10 <- runif(100, 0, 1) * seq(1:100)
  dfS <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  main <- lag_pc_test(dfS, 1, 1, T, 0.1, 1, T, T)
  expect_equal(main[[1]], 1)
})
