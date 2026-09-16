#' Two-parameter ridge with a formula
#'
#' A formula interface to [tpr_fit()], so the estimator can be used on
#' any regression in the way `lm()` is used. Nothing about the
#' two-parameter ridge is specific to volatility models: it is a
#' general remedy for an ill-conditioned design.
#'
#' @param formula A model formula, as for [stats::lm()].
#' @param data A data frame, list or environment holding the variables.
#' @param lambda Penalty. If `NULL` it is chosen by `rule`.
#' @param rule Either `"hkb"`, the data-driven rule of Hoerl, Kennard
#'   and Baldwin, or `"df"`, which solves for the penalty delivering
#'   `target_df` effective degrees of freedom.
#' @param target_df Target effective degrees of freedom, used when
#'   `rule = "df"`.
#' @param subset Optional vector of rows to use.
#' @param na.action How to treat missing values. Defaults to
#'   [stats::na.omit()].
#' @param standardize Standardize the regressors before estimation.
#'   Default `TRUE`, and recommended: the penalty is not scale
#'   invariant.
#' @return An object of class `tpr_lm`, inheriting from `tpr_fit`, with
#'   the call, terms and model frame attached so that `predict()` and
#'   `summary()` behave as expected.
#' @seealso [tpr_fit()] for the matrix interface, [summary.tpr_lm()]
#'   for the coefficient table and collinearity diagnostics.
#' @examples
#' ## The classic ill-conditioned design of Longley (1967)
#' data(longley)
#' m <- tpr_lm(Employed ~ GNP + Unemployed + Armed.Forces +
#'               Population + Year, data = longley)
#' m
#' summary(m)
#' coef(m, "Ridge-II")
#' @export
tpr_lm <- function(formula, data, lambda = NULL, rule = c("hkb", "df"),
                   target_df = NULL, subset = NULL, na.action = stats::na.omit,
                   standardize = TRUE) {
  rule <- match.arg(rule)
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m  <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")

  y <- stats::model.response(mf, "numeric")
  if (is.null(y)) stop("`formula` must have a numeric response.")
  X <- stats::model.matrix(mt, mf)
  icept <- which(colnames(X) == "(Intercept)")
  if (length(icept)) X <- X[, -icept, drop = FALSE]
  if (!ncol(X)) stop("The model has no regressors.")

  fit <- tpr_fit(X, y, lambda = lambda, rule = rule,
                 target_df = target_df, standardize = standardize)
  fit$call <- cl
  fit$terms <- mt
  fit$model <- mf
  fit$formula <- formula
  fit$xlevels <- stats::.getXlevels(mt, mf)
  fit$residuals <- lapply(names(fit$coefficients_std), function(e)
    as.numeric(y - (fit$window$center_y +
                    fit$window$Z %*% fit$coefficients_std[[e]])))
  names(fit$residuals) <- names(fit$coefficients_std)
  class(fit) <- c("tpr_lm", "tpr_fit")
  fit
}

#' Summarise a two-parameter ridge regression
#'
#' Reports the coefficients of all three estimators side by side,
#' together with the diagnostics that bear on whether shrinkage is
#' called for: the variance inflation factors, the condition number of
#' the design, the penalty, the scalar `q` and the effective degrees of
#' freedom.
#'
#' No standard errors are given. The sampling distribution of a
#' shrinkage estimator is not that of least squares, so the usual
#' standard errors and the tests built on them do not carry over.
#' Inference for the ridge estimators would require a bootstrap.
#'
#' @param object An object of class `tpr_lm`.
#' @param ... Ignored.
#' @return An object of class `summary.tpr_lm`, invisibly printed.
#' @examples
#' data(longley)
#' summary(tpr_lm(Employed ~ GNP + Unemployed + Year, data = longley))
#' @export
summary.tpr_lm <- function(object, ...) {
  w <- object$window
  ## variance inflation factors from the standardized cross-product
  R <- w$G / (w$n - 1)
  vif <- tryCatch(diag(solve(stats::cov2cor(R))),
                  error = function(e) rep(NA_real_, w$p))
  names(vif) <- colnames(w$Z)
  out <- list(call = object$call,
              coefficients = do.call(cbind, object$coefficients),
              vif = vif, condition = object$condition,
              eigen_min = object$eigen_min,
              lambda = object$lambda, q = object$q,
              eff_df = object$eff_df, p = object$p, n = object$n,
              r2_in = object$r2_in, recovery = object$recovery,
              gap = object$gap_empirical,
              residual_se = sqrt(object$sigma2))
  class(out) <- "summary.tpr_lm"
  out
}

#' @export
print.summary.tpr_lm <- function(x, ...) {
  cat("\nCall:\n"); print(x$call)
  cat("\nCoefficients:\n")
  print(round(x$coefficients, 6))
  cat("\nCollinearity diagnostics:\n")
  cat(sprintf("  condition number of X'X      %.1f\n", x$condition))
  cat(sprintf("  smallest eigenvalue          %.4g\n", x$eigen_min))
  cat("  variance inflation factors\n")
  for (nm in names(x$vif))
    cat(sprintf("    %-20s %8.2f%s\n", nm, x$vif[[nm]],
                if (is.finite(x$vif[[nm]]) && x$vif[[nm]] > 10) "  *" else ""))
  cat("\nShrinkage:\n")
  cat(sprintf("  lambda %.4g,  q %.6f,  effective df %.3f of %d\n",
              x$lambda, x$q, x$eff_df, x$p))
  cat("\nIn-sample R2:\n")
  for (nm in names(x$r2_in))
    cat(sprintf("  %-9s %.6f\n", nm, x$r2_in[[nm]]))
  cat(sprintf("\nFit recovered by the second parameter: %.1f%% (%.3e in R2)\n",
              100 * x$recovery, x$gap))
  cat(sprintf("Residual standard error: %.4g on %d degrees of freedom\n",
              x$residual_se, x$n - x$p - 1L))
  cat("\nStandard errors are not reported: the sampling distribution of a\n")
  cat("shrinkage estimator differs from that of least squares.\n")
  if (any(x$vif > 10, na.rm = TRUE))
    cat("\n* variance inflation factor above 10, the conventional threshold\n")
  invisible(x)
}

#' @export
print.tpr_lm <- function(x, ...) {
  cat("\nCall:\n"); print(x$call)
  cat(sprintf("\nlambda %.4g   q %.6f   effective df %.3f of %d",
              x$lambda, x$q, x$eff_df, x$p))
  cat(sprintf("   condition %.1f\n\n", x$condition))
  print(round(do.call(cbind, x$coefficients), 6))
  invisible(x)
}

#' @export
predict.tpr_lm <- function(object, newdata, estimator = "Ridge-II", ...) {
  estimator <- match.arg(estimator, names(object$coefficients))
  if (missing(newdata) || is.null(newdata))
    return(as.numeric(object$window$center_y +
             object$window$Z %*% object$coefficients_std[[estimator]]))
  mt <- stats::delete.response(object$terms)
  mf <- stats::model.frame(mt, newdata, na.action = stats::na.pass,
                           xlev = object$xlevels)
  X <- stats::model.matrix(mt, mf)
  icept <- which(colnames(X) == "(Intercept)")
  if (length(icept)) X <- X[, -icept, drop = FALSE]
  b <- object$coefficients[[estimator]]
  as.numeric(b[1] + X %*% b[-1])
}

#' @export
residuals.tpr_lm <- function(object, estimator = "Ridge-II", ...) {
  estimator <- match.arg(estimator, names(object$residuals))
  object$residuals[[estimator]]
}
