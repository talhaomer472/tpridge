## ---------------------------------------------------------------------
##  Plot methods. Base graphics only, so the package has no hard
##  dependency on ggplot2.
## ---------------------------------------------------------------------

.pal <- c(OLS = "#1a1a1a", `Ridge-I` = "#0b5394", `Ridge-II` = "#cc0000")
.pch <- c(OLS = 16L, `Ridge-I` = 15L, `Ridge-II` = 17L)

#' Plot a fitted model
#'
#' @param x An object of class `tpr_fit`.
#' @param which One of `"path"`, the coefficient of determination of all
#'   three estimators against the effective degrees of freedom;
#'   `"coef"`, the standardized coefficients; or `"gap"`, the fit gain
#'   of Proposition 2 against the penalty.
#' @param n_grid Number of penalty values on the path.
#' @param ... Passed to the underlying plotting calls.
#' @return Invisibly, the data behind the plot.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' y <- as.numeric(X %*% c(1, 0.5, 0.2)) + rnorm(100)
#' plot(tpr_fit(X, y), which = "path")
#' @export
plot.tpr_fit <- function(x, which = c("path", "coef", "gap"),
                         n_grid = 40L, ...) {
  which <- match.arg(which)
  w <- x$window; p <- w$p
  tgt <- seq(p * 0.999, 0.05 * p, length.out = n_grid)
  lam <- vapply(tgt, function(t) .lambda_for_df(w$eigenvalues, t), 0)
  M <- t(vapply(lam, function(l) {
    s <- .ridge_solve(w, l)
    c(OLS = 1 - sum((w$yc - w$Z %*% w$b_ols)^2) / w$tss,
      `Ridge-I`  = 1 - sum((w$yc - w$Z %*% s$b_I)^2) / w$tss,
      `Ridge-II` = 1 - sum((w$yc - w$Z %*% s$b_II)^2) / w$tss,
      q = s$q, gap = (s$q - 1)^2 * s$Q2 / w$tss)
  }, numeric(5)))
  d <- data.frame(eff_df = tgt, lambda = lam, M)

  if (which == "path") {
    graphics::plot(d$eff_df, d$OLS, type = "n", xlim = rev(range(d$eff_df)),
      ylim = range(d[, 3:5]), xlab = "Effective degrees of freedom",
      ylab = expression(R^2), font.lab = 2, ...)
    for (nm in names(.pal)) {
      graphics::lines(d$eff_df, d[[make.names(nm)]], col = .pal[nm], lwd = 2.2)
      graphics::points(d$eff_df, d[[make.names(nm)]], col = .pal[nm],
                       pch = .pch[nm], cex = 0.7)
    }
    graphics::legend("bottomleft", legend = names(.pal), col = .pal,
                     pch = .pch, lwd = 2.2, bty = "n")
    graphics::abline(v = x$eff_df, lty = 3)
  } else if (which == "gap") {
    graphics::plot(d$eff_df, d$gap, type = "l", lwd = 2.2, col = .pal[3],
      xlim = rev(range(d$eff_df)), xlab = "Effective degrees of freedom",
      ylab = "Fit gain of Ridge-II over Ridge-I", font.lab = 2, ...)
    graphics::abline(h = 0, col = "grey50")
  } else {
    b <- do.call(cbind, x$coefficients_std)
    graphics::barplot(t(b), beside = TRUE, col = .pal,
      names.arg = colnames(w$Z), las = 2,
      ylab = "Standardized coefficient", font.lab = 2, ...)
    graphics::legend("topright", legend = names(.pal), fill = .pal, bty = "n")
  }
  invisible(d)
}

#' Plot rolling forecasts
#'
#' @param x An object of class `tpr_rolling`.
#' @param which One of `"forecast"`, the realization against the
#'   forecast; `"q"`, the two-parameter scalar over time; `"lambda"`,
#'   the penalty and the effective degrees of freedom; or `"path"`, the
#'   out-of-sample fit against the effective degrees of freedom, which
#'   requires the object to have been produced with `rule = "df"`.
#' @param estimator Estimator to display where only one is shown.
#' @param log Plot on a logarithmic vertical scale.
#' @param ... Passed to the underlying plotting calls.
#' @return Invisibly, the data behind the plot.
#' @examples
#' set.seed(1)
#' des <- har_design(har_features(har_simulate(600, sigma2 = 0.5)))
#' r <- tpr_rolling(des$X, des$y, window = 250)
#' plot(r, which = "q")
#' @export
plot.tpr_rolling <- function(x, which = c("forecast", "q", "lambda", "path"),
                             estimator = "Ridge-II", log = TRUE, ...) {
  which <- match.arg(which)
  d <- as.data.frame(x)
  xs <- if (all(is.na(d$date))) d$origin else d$date

  if (which == "forecast") {
    s <- d[d$estimator == estimator & d$rule == d$rule[1], ]
    xv <- if (all(is.na(s$date))) s$origin else s$date
    a <- if (all(is.na(s$actual_level))) s$actual else s$actual_level
    f <- if (all(is.na(s$pred_level))) s$pred else s$pred_level
    graphics::plot(xv, a, type = "l", col = "grey45", lwd = 0.7,
      log = if (log && all(a > 0, na.rm = TRUE)) "y" else "",
      xlab = NULL, ylab = "Realized variance", font.lab = 2, ...)
    graphics::lines(xv, f, col = .pal[["Ridge-II"]], lwd = 0.8)
    graphics::legend("topright", legend = c("Actual", estimator),
      col = c("grey45", .pal[["Ridge-II"]]), lwd = 2, bty = "n")
  } else if (which == "q") {
    s <- d[d$estimator == "Ridge-II", ]
    xv <- if (all(is.na(s$date))) s$origin else s$date
    graphics::plot(xv, s$q, type = "l", col = .pal[["Ridge-II"]], lwd = 0.8,
      xlab = NULL, ylab = "q", font.lab = 2, ...)
    graphics::abline(h = 1, lty = 2, col = "grey40")
  } else if (which == "lambda") {
    s <- d[d$estimator == "Ridge-II", ]
    xv <- if (all(is.na(s$date))) s$origin else s$date
    op <- graphics::par(mfrow = c(2, 1), mar = c(3, 4.2, 1, 1))
    on.exit(graphics::par(op), add = TRUE)
    graphics::plot(xv, s$lambda, type = "l", lwd = 0.8, log = "y",
      ylab = expression(lambda), xlab = NULL, font.lab = 2)
    graphics::plot(xv, s$eff_df, type = "l", lwd = 0.8,
      ylab = "Effective df", xlab = NULL, font.lab = 2)
  } else {
    if (all(is.na(d$target_df)))
      stop("A penalty path requires rule = \"df\" in tpr_rolling().")
    s <- do.call(rbind, lapply(split(d, list(d$estimator, d$target_df)),
      function(g) data.frame(estimator = g$estimator[1],
        eff_df = mean(g$eff_df),
        r2 = 1 - sum((g$actual - g$pred)^2) /
                 sum((g$actual - g$prev_mean)^2))))
    graphics::plot(s$eff_df, s$r2, type = "n", xlim = rev(range(s$eff_df)),
      xlab = "Effective degrees of freedom",
      ylab = expression("Out-of-sample"~R^2), font.lab = 2, ...)
    for (nm in names(.pal)) {
      g <- s[s$estimator == nm, ]; g <- g[order(-g$eff_df), ]
      graphics::lines(g$eff_df, g$r2, col = .pal[nm], lwd = 2.2)
      graphics::points(g$eff_df, g$r2, col = .pal[nm], pch = .pch[nm])
    }
    graphics::legend("bottomleft", legend = names(.pal), col = .pal,
                     pch = .pch, lwd = 2.2, bty = "n")
    return(invisible(s))
  }
  invisible(d)
}

#' Plot a Monte Carlo study
#'
#' @param x An object of class `tpr_mc`.
#' @param which One of `"r2"`, the out-of-sample fit by sample size;
#'   `"recovery"`, the share of the sacrificed fit that the second
#'   parameter restores; or `"identity"`, the empirical fit gain against
#'   its theoretical value.
#' @param ... Passed to the underlying plotting calls.
#' @return Invisibly, the data behind the plot.
#' @examples
#' \donttest{
#' mc <- tpr_montecarlo(n = c(100, 200), sigma2 = 0.5, reps = 20, seed = 1)
#' plot(mc, which = "identity")
#' }
#' @export
plot.tpr_mc <- function(x, which = c("r2", "recovery", "identity"), ...) {
  which <- match.arg(which)
  d <- as.data.frame(x)
  if (which == "identity") {
    graphics::plot(d$gap_theoretical, d$gap_empirical, pch = 16, cex = 0.4,
      col = grDevices::adjustcolor(.pal[["Ridge-II"]], alpha.f = 0.3),
      xlab = "Theoretical gain", ylab = "Empirical gain", font.lab = 2, ...)
    graphics::abline(0, 1, col = "grey40", lwd = 1.2)
  } else if (which == "r2") {
    s <- stats::aggregate(r2_oos ~ n + estimator, data = d, FUN = mean)
    graphics::plot(range(s$n), range(s$r2_oos), type = "n",
      xlab = "Sample size", ylab = expression("Out-of-sample"~R^2),
      font.lab = 2, ...)
    for (nm in names(.pal)) {
      g <- s[s$estimator == nm, ]; g <- g[order(g$n), ]
      graphics::lines(g$n, g$r2_oos, col = .pal[nm], lwd = 2.2)
      graphics::points(g$n, g$r2_oos, col = .pal[nm], pch = .pch[nm])
    }
    graphics::legend("bottomright", legend = names(.pal), col = .pal,
                     pch = .pch, lwd = 2.2, bty = "n")
  } else {
    w <- stats::reshape(d[, c("rep", "n", "sigma2", "estimator", "r2_in")],
      idvar = c("rep", "n", "sigma2"), timevar = "estimator",
      direction = "wide")
    rec <- 100 * (w$`r2_in.Ridge-II` - w$`r2_in.Ridge-I`) /
                 (w$r2_in.OLS - w$`r2_in.Ridge-I`)
    s <- stats::aggregate(rec ~ w$n, FUN = function(z) mean(z, na.rm = TRUE))
    names(s) <- c("n", "recovery")
    graphics::barplot(s$recovery, names.arg = s$n, ylim = c(0, 100),
      col = .pal[["Ridge-II"]], xlab = "Sample size",
      ylab = "Fit recovery (per cent)", font.lab = 2, ...)
  }
  invisible(d)
}
