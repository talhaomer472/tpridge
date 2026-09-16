#' Rolling one-step-ahead forecasts
#'
#' Refits all three estimators in every window and returns one row per
#' forecast origin per estimator. The window is decomposed once and
#' every penalty rule requested is evaluated on that decomposition,
#' which is what makes a path over several penalties inexpensive.
#'
#' @param X Numeric matrix of regressors.
#' @param y Numeric response, aligned with the rows of `X`.
#' @param window Width of the rolling estimation window.
#' @param date Optional vector of dates for the rows of `X`.
#' @param rule Penalty rule, `"hkb"` or `"df"`.
#' @param target_df Either a single target or a vector of targets, used
#'   when `rule = "df"`. When several are supplied, all are applied to
#'   the same window, so the comparison across penalties is paired.
#' @param log_target The response is on the logarithmic scale, so
#'   forecasts are also returned on the level scale with the correction
#'   \eqn{\exp(\hat f + \hat\sigma^2/2)}. Default `TRUE`.
#' @param progress Print progress every 500 windows.
#' @return An object of class `tpr_rolling`, a `data.table` with the
#'   forecast, the realization, the penalty, `q`, the effective degrees
#'   of freedom, the condition number, the in-sample fit and the two
#'   fit-gain quantities of Proposition 2.
#' @examples
#' set.seed(1)
#' d <- har_features(har_simulate(600, sigma2 = 0.5))
#' des <- har_design(d)
#' r <- tpr_rolling(des$X, des$y, window = 250)
#' summary(r)
#' @export
tpr_rolling <- function(X, y, window, date = NULL, rule = c("hkb", "df"),
                        target_df = NULL, log_target = TRUE,
                        progress = FALSE) {
  rule <- match.arg(rule)
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  if (window >= n) stop("`window` must be shorter than the sample.")
  nf <- n - window
  if (nf < 10L) stop("Fewer than ten forecasts would be produced.")
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(p))
  rules <- if (rule == "hkb") list(list(tag = "hkb", tdf = NA_real_)) else {
    if (is.null(target_df)) stop("`target_df` is required when rule = \"df\".")
    lapply(target_df, function(t) list(tag = sprintf("df%.2f", t), tdf = t))
  }
  est <- c("OLS", "Ridge-I", "Ridge-II")
  res <- vector("list", nf)

  for (j in seq_len(nf)) {
    i  <- window + j
    w  <- tpr_window(X[(i - window):(i - 1L), , drop = FALSE],
                     y[(i - window):(i - 1L)])
    xn <- X[i, ]
    z  <- (xn - w$center_x) / w$scale
    res[[j]] <- data.table::rbindlist(lapply(rules, function(rl) {
      lam <- if (is.na(rl$tdf)) .lambda_hkb(w) else
             .lambda_for_df(w$eigenvalues, rl$tdf)
      s <- .ridge_solve(w, lam)
      bb <- list(OLS = w$b_ols, `Ridge-I` = s$b_I, `Ridge-II` = s$b_II)
      r2 <- vapply(bb, function(b) 1 - sum((w$yc - w$Z %*% b)^2) / w$tss, 0)
      fl <- vapply(bb, function(b) w$center_y + sum(z * b), 0)
      data.table::data.table(
        origin = i - 1L,
        date = if (is.null(date)) NA else date[i],
        rule = rl$tag, target_df = rl$tdf, estimator = est,
        actual = y[i], pred = unname(fl[est]),
        pred_level = if (log_target)
          exp(unname(fl[est]) + w$sigma2 / 2) else NA_real_,
        actual_level = if (log_target) exp(y[i]) else NA_real_,
        lambda = lam, q = s$q, rho = s$rho,
        eff_df = tpr_eff_df(w$eigenvalues, lam),
        condition = max(w$eigenvalues) / min(w$eigenvalues),
        r2_in = unname(r2[est]),
        gap_empirical = unname(r2[["Ridge-II"]] - r2[["Ridge-I"]]),
        gap_theoretical = (s$q - 1)^2 * s$Q2 / w$tss)
    }))
    if (progress && j %% 500L == 0L)
      message(sprintf("  window %d of %d", j, nf))
  }
  out <- data.table::rbindlist(res)
  ## prevailing-mean benchmark, using only information already available
  pm <- vapply(seq_len(nf), function(j) mean(y[seq_len(window + j - 1L)]), 0)
  bm <- data.table::data.table(origin = window + seq_len(nf) - 1L,
                               prev_mean = pm)
  out <- merge(out, bm, by = "origin", all.x = TRUE)
  structure(out, class = c("tpr_rolling", class(out)),
            window = window, p = p, log_target = log_target)
}
