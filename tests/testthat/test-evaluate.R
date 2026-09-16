test_that("the R2 and MSE identity holds", {
  set.seed(1)
  a <- rnorm(200); f <- a + rnorm(200, sd = 0.4)
  b <- rep(mean(a), 200)
  m <- tpr_metrics(a, f, benchmark = b)
  expect_equal(m$OOS_R2, 1 - m$MSE * m$N / sum((a - b)^2), tolerance = 1e-12)
})

test_that("Diebold and Mariano is symmetric under a swap", {
  set.seed(2)
  la <- rchisq(300, 2); lb <- rchisq(300, 2.4)
  s1 <- tpr_dm(la, lb); s2 <- tpr_dm(lb, la)
  expect_equal(unname(s1["DM"]), unname(-s2["DM"]), tolerance = 1e-10)
  expect_equal(unname(s1["p"]), unname(s2["p"]), tolerance = 1e-10)
})

test_that("a correctly specified VaR is not rejected", {
  set.seed(3)
  v <- rep(4e-4, 4000)
  r <- rnorm(4000, sd = sqrt(v))
  bt <- tpr_var_backtest(v, r, alpha = 0.05, dist = "normal")
  expect_gt(bt$kupiec_p, 0.01)
  expect_lt(abs(bt$rate - 5), 1.5)
})

test_that("printing a Monte Carlo object does not recurse", {
  skip_on_cran()
  set.seed(9)
  mc <- tpr_montecarlo(n = 120, sigma2 = 0.5, reps = 3, progress = FALSE)
  expect_output(print(mc), "Monte Carlo study")
  expect_s3_class(summary_out <- mc, "tpr_mc")
})

test_that("summarising rolling forecasts returns a plain table", {
  set.seed(10)
  des <- har_design(har_features(har_simulate(300, 0.5)))
  s <- summary(tpr_rolling(des$X, des$y, window = 120))
  expect_false(inherits(s, "tpr_rolling"))
  expect_output(print(s))
})
