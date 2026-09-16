#' Print a fitted model
#'
#' @param x An object of class `tpr_fit`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' print(tpr_fit(matrix(rnorm(300), ncol = 3), rnorm(100)))
#' @export
print.tpr_fit <- function(x, ...) {
  cat("Two-parameter ridge fit\n")
  cat(sprintf("  observations %d, predictors %d\n", x$n, x$p))
  cat(sprintf("  lambda %.4g   q %.6f   effective df %.3f of %d\n",
              x$lambda, x$q, x$eff_df, x$p))
  cat(sprintf("  condition number %.1f\n", x$condition))
  cat("\n  in-sample R2\n")
  for (nm in names(x$r2_in))
    cat(sprintf("    %-9s %.6f\n", nm, x$r2_in[[nm]]))
  cat(sprintf("\n  fit gain (Ridge-II over Ridge-I) %.3e\n", x$gap_empirical))
  cat(sprintf("  Proposition 2 residual           %.2e\n",
              abs(x$gap_empirical - x$gap_theoretical)))
  if (is.finite(x$recovery))
    cat(sprintf("  recovery of sacrificed fit       %.1f%%\n",
                100 * x$recovery))
  invisible(x)
}

#' Coefficients of a fitted model
#'
#' @param object An object of class `tpr_fit`.
#' @param estimator One of `"OLS"`, `"Ridge-I"` or `"Ridge-II"`.
#' @param ... Ignored.
#' @return A named numeric vector on the original scale, including the
#'   intercept.
#' @examples
#' set.seed(1)
#' f <- tpr_fit(matrix(rnorm(300), ncol = 3), rnorm(100))
#' coef(f, "Ridge-I")
#' @export
coef.tpr_fit <- function(object, estimator = "Ridge-II", ...) {
  estimator <- match.arg(estimator, names(object$coefficients))
  object$coefficients[[estimator]]
}

#' Fitted values of a fitted model
#'
#' @inheritParams coef.tpr_fit
#' @return A numeric vector of in-sample fitted values.
#' @examples
#' set.seed(1)
#' f <- tpr_fit(matrix(rnorm(300), ncol = 3), rnorm(100))
#' head(fitted(f))
#' @export
fitted.tpr_fit <- function(object, estimator = "Ridge-II", ...) {
  estimator <- match.arg(estimator, names(object$coefficients_std))
  w <- object$window
  as.numeric(w$center_y + w$Z %*% object$coefficients_std[[estimator]])
}

#' Forecast from a fitted model
#'
#' @inheritParams coef.tpr_fit
#' @param newdata Matrix of new regressor values, with the same columns
#'   as the design used to fit.
#' @param level Return the forecast on the level scale, applying
#'   \eqn{\exp(\hat f + \hat\sigma^2/2)}. Appropriate only when the
#'   model was fitted to a logarithm.
#' @return A numeric vector of forecasts.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3); y <- rnorm(100)
#' f <- tpr_fit(X[1:90, ], y[1:90])
#' predict(f, X[91:100, ])
#' @export
predict.tpr_fit <- function(object, newdata, estimator = "Ridge-II",
                            level = FALSE, ...) {
  estimator <- match.arg(estimator, names(object$coefficients_std))
  w <- object$window
  Xn <- as.matrix(newdata)
  if (ncol(Xn) != w$p) stop("`newdata` must have ", w$p, " columns.")
  Z <- (Xn - rep(w$center_x, each = nrow(Xn))) / rep(w$scale, each = nrow(Xn))
  f <- as.numeric(w$center_y + Z %*% object$coefficients_std[[estimator]])
  if (level) exp(f + object$sigma2 / 2) else f
}

#' Print rolling forecasts
#'
#' @param x An object of class `tpr_rolling`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' des <- har_design(har_features(har_simulate(400, 0.5)))
#' print(tpr_rolling(des$X, des$y, window = 150))
#' @export
print.tpr_rolling <- function(x, ...) {
  cat("Rolling forecasts\n")
  cat(sprintf("  window %d, predictors %d, origins %d\n",
              attr(x, "window"), attr(x, "p"), length(unique(x$origin))))
  cat(sprintf("  penalty rules: %s\n", paste(unique(x$rule), collapse = ", ")))
  invisible(x)
}

#' Summarise rolling forecasts
#'
#' @param object An object of class `tpr_rolling`.
#' @param ... Ignored.
#' @return A `data.table` with one row per penalty rule and estimator,
#'   giving the mean squared error, the out-of-sample coefficient of
#'   determination against the prevailing mean, the mean in-sample fit,
#'   the median penalty, and the mean values of `q` and the effective
#'   degrees of freedom.
#' @examples
#' set.seed(1)
#' des <- har_design(har_features(har_simulate(400, 0.5)))
#' summary(tpr_rolling(des$X, des$y, window = 150))
#' @export
summary.tpr_rolling <- function(object, ...) {
  s <- object[, {
    e <- actual - pred
    list(N = .N, MSE = mean(e^2),
         OOS_R2 = 1 - sum(e^2) / sum((actual - prev_mean)^2),
         r2_in = mean(r2_in), lambda = stats::median(lambda),
         q = mean(q), eff_df = mean(eff_df))
  }, by = list(rule, estimator)]
  data.table::setorder(s, rule, estimator)
  ## drop the "tpr_rolling" class that `[` carried over, so the result
  ## prints as an ordinary table
  data.table::setattr(s, "class", c("data.table", "data.frame"))
  s[]
}

#' Print a Monte Carlo study
#'
#' @param x An object of class `tpr_mc`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' print(tpr_montecarlo(n = 150, sigma2 = 0.5, reps = 5, progress = FALSE))
#' }
#' @export
print.tpr_mc <- function(x, ...) {
  cat("Monte Carlo study\n")
  cat(sprintf("  cells %d, replications %d\n",
              nrow(unique(x[, list(n, sigma2)])), attr(x, "reps")))
  cat(sprintf("  q exceeds one in %.1f%% of replications, minimum %.6f\n",
              100 * mean(x$q_min > 1), min(x$q_min)))
  cat(sprintf("  largest Proposition 2 residual %.2e\n",
              max(abs(x$gap_empirical - x$gap_theoretical))))
  ## data.table's `[` preserves the class of x, so the aggregate would
  ## inherit "tpr_mc" and printing it would re-enter this method. Strip
  ## the class before printing.
  s <- x[, list(r2_oos = mean(r2_oos), r2_in = mean(r2_in)),
         by = list(n, estimator)]
  data.table::setorder(s, n, estimator)
  print(as.data.frame(s))
  invisible(x)
}
