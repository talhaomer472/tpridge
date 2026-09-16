test_that("the simulated process is strictly positive", {
  set.seed(1)
  for (s in c(0.1, 0.5, 1.2))
    expect_true(all(har_simulate(500, s) > 0))
})

test_that("weekly and monthly components are the stated averages", {
  set.seed(2)
  rv <- har_simulate(300, 0.5)
  d <- har_features(rv, log = FALSE)
  i <- 60
  expect_equal(d$RV_5[i], mean(rv[(i - 4):i]), tolerance = 1e-12)
  expect_equal(d$RV_22[i], mean(rv[(i - 21):i]), tolerance = 1e-12)
  expect_equal(d$y[i], rv[i + 1], tolerance = 1e-12)
})

test_that("rolling forecasts have the right length", {
  set.seed(3)
  des <- har_design(har_features(har_simulate(400, 0.5)))
  r <- tpr_rolling(des$X, des$y, window = 150)
  expect_equal(length(unique(r$origin)), nrow(des$X) - 150)
  expect_setequal(unique(r$estimator), c("OLS", "Ridge-I", "Ridge-II"))
})

test_that("the penalty path is paired within a replication", {
  set.seed(4)
  des <- har_design(har_features(har_simulate(400, 0.5)))
  r <- tpr_rolling(des$X, des$y, window = 150, rule = "df",
                   target_df = c(3, 2, 1))
  ols <- r[r$estimator == "OLS", ]
  by_origin <- split(ols$pred, ols$origin)
  expect_true(all(vapply(by_origin,
                         function(z) diff(range(z)) < 1e-10, TRUE)))
})
