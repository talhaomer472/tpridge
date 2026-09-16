test_that("ridge at lambda zero reproduces least squares", {
  set.seed(1)
  X <- matrix(rnorm(300), ncol = 3)
  y <- as.numeric(X %*% c(1, 0.5, 0.2)) + rnorm(100)
  f <- tpr_fit(X, y, lambda = 0)
  expect_equal(unname(coef(f, "OLS")), unname(coef(lm(y ~ X))),
               tolerance = 1e-8)
})

test_that("Proposition 1: q exceeds one for every positive penalty", {
  set.seed(2)
  X <- matrix(rnorm(600), ncol = 4)
  y <- as.numeric(X %*% c(1, 0.5, 0.2, 0.1)) + rnorm(150)
  for (l in c(1, 5, 20, 100, 1000, 1e5)) {
    f <- tpr_fit(X, y, lambda = l)
    expect_gt(f$q, 1)
  }
})

test_that("Proposition 2: the fit gain equals its closed form", {
  set.seed(3)
  X <- matrix(rnorm(600), ncol = 4)
  y <- as.numeric(X %*% c(1, 0.5, 0.2, 0.1)) + rnorm(150)
  for (l in c(1, 20, 500, 5000)) {
    f <- tpr_fit(X, y, lambda = l)
    expect_equal(f$gap_empirical, f$gap_theoretical, tolerance = 1e-12)
  }
})

test_that("Proposition 3: the fit ordering holds", {
  set.seed(4)
  X <- matrix(rnorm(600), ncol = 4)
  y <- as.numeric(X %*% c(1, 0.5, 0.2, 0.1)) + rnorm(150)
  for (l in c(1, 20, 500, 5000)) {
    f <- tpr_fit(X, y, lambda = l)
    expect_gte(f$r2_in[["OLS"]], f$r2_in[["Ridge-II"]] - 1e-12)
    expect_gte(f$r2_in[["Ridge-II"]], f$r2_in[["Ridge-I"]] - 1e-12)
  }
})

test_that("Proposition 4: Ridge-II converges to a finite non-zero limit", {
  set.seed(5)
  X <- matrix(rnorm(400), ncol = 4)
  y <- as.numeric(X %*% c(1, 0.5, 0.2, 0.1)) + rnorm(100)
  w <- tpr_window(X, y)
  lim <- as.numeric(sum(w$r^2) / crossprod(w$r, w$G %*% w$r)) * w$r
  f <- tpr_fit(X, y, lambda = 1e10)
  expect_equal(f$coefficients_std[["Ridge-II"]], lim, tolerance = 1e-6)
  expect_lt(max(abs(f$coefficients_std[["Ridge-I"]])), 1e-6)
})

test_that("effective degrees of freedom span p to zero", {
  set.seed(6)
  X <- matrix(rnorm(300), ncol = 3)
  expect_equal(tpr_eff_df(X, 0), 3)
  expect_lt(tpr_eff_df(X, 1e12), 1e-6)
  l <- tpr_lambda(X, rnorm(100), rule = "df", target_df = 1.5)
  expect_equal(tpr_eff_df(X, l), 1.5, tolerance = 1e-6)
})

test_that("the formula interface matches the matrix interface", {
  data(longley, package = "datasets")
  v <- c("GNP", "Unemployed", "Armed.Forces", "Population", "Year")
  m <- tpr_lm(Employed ~ GNP + Unemployed + Armed.Forces + Population + Year,
              data = longley)
  f <- tpr_fit(as.matrix(longley[, v]), longley$Employed)
  expect_equal(unname(coef(m, "Ridge-II")), unname(coef(f, "Ridge-II")),
               tolerance = 1e-10)
  expect_equal(m$q, f$q, tolerance = 1e-12)
})

test_that("predict on the fitting data reproduces the fitted values", {
  data(longley, package = "datasets")
  m <- tpr_lm(Employed ~ GNP + Unemployed + Year, data = longley)
  expect_equal(predict(m, longley), predict(m), tolerance = 1e-10)
})

test_that("summary reports variance inflation on a collinear design", {
  data(longley, package = "datasets")
  s <- summary(tpr_lm(Employed ~ GNP + Unemployed + Armed.Forces +
                        Population + Year, data = longley))
  expect_true(max(s$vif, na.rm = TRUE) > 10)
  expect_gt(s$condition, 100)
})
