#' Monte Carlo study of the three estimators
#'
#' Reproduces the design of the accompanying paper. For each cell of the
#' grid the process of [har_simulate()] is drawn afresh, split into an
#' estimation and an evaluation part, and all three estimators are
#' rolled forward. When several penalty targets are supplied they are
#' applied to the same simulated path within a replication, so the
#' comparison across penalties is paired.
#'
#' @param n Sample sizes to consider.
#' @param sigma2 Variances of the multiplicative shock.
#' @param reps Replications per cell.
#' @param train_frac Share of each sample used for estimation.
#' @param rule Penalty rule, `"hkb"` or `"df"`.
#' @param target_df Targets used when `rule = "df"`.
#' @param alpha,betas Parameters of the data-generating process.
#' @param seed Seed for reproducibility.
#' @param progress Report each cell as it completes.
#' @return An object of class `tpr_mc`, a `data.table` with one row per
#'   replication, cell and estimator.
#' @examples
#' \donttest{
#' mc <- tpr_montecarlo(n = 200, sigma2 = 0.5, reps = 20, seed = 1)
#' print(mc)
#' }
#' @export
tpr_montecarlo <- function(n = c(100, 200, 400, 500),
                           sigma2 = c(0.1, 0.4, 0.5, 0.6, 0.7, 0.8,
                                      1.0, 1.1, 1.2),
                           reps = 1000L, train_frac = 0.4,
                           rule = c("hkb", "df"),
                           target_df = c(3, 2.5, 2, 1.5, 1, 0.5),
                           alpha = 0.1, betas = c(0.3, 0.3, 0.3),
                           seed = NULL, progress = TRUE) {
  rule <- match.arg(rule)
  if (!is.null(seed)) set.seed(seed)
  beta_true <- c(alpha, betas)
  grid <- expand.grid(n = n, sigma2 = sigma2, KEEP.OUT.ATTRS = FALSE)
  res <- vector("list", nrow(grid) * reps); k <- 0L

  for (g in seq_len(nrow(grid))) {
    nn <- grid$n[g]; s2 <- grid$sigma2[g]
    W <- floor(train_frac * nn)
    for (b in seq_len(reps)) {
      rv <- har_simulate(nn, s2, alpha, betas)
      d  <- har_features(rv, log = FALSE)
      des <- har_design(d)
      if (nrow(des$X) - W < 10L) next
      r <- tpr_rolling(des$X, des$y, window = W, rule = rule,
                       target_df = if (rule == "df") target_df else NULL,
                       log_target = FALSE)
      agg <- r[, {
        e <- actual - pred
        pm <- prev_mean
        list(r2_in = mean(r2_in), msfe = mean(e^2),
             r2_oos = 1 - sum(e^2) / sum((actual - pm)^2),
             lambda = mean(lambda), q = mean(q), q_min = min(q),
             eff_df = mean(eff_df),
             gap_empirical = mean(gap_empirical),
             gap_theoretical = mean(gap_theoretical),
             n_negative = sum(pred <= 0))
      }, by = list(rule, target_df, estimator)]
      data.table::setattr(agg, "class", c("data.table", "data.frame"))
      agg[, `:=`(rep = b, n = nn, sigma2 = s2, W = W)]
      k <- k + 1L; res[[k]] <- agg
    }
    if (progress) message(sprintf("  n = %d, sigma2 = %.1f  done", nn, s2))
  }
  out <- data.table::rbindlist(res[seq_len(k)])
  structure(out, class = c("tpr_mc", class(out)),
            beta_true = beta_true, reps = reps)
}
