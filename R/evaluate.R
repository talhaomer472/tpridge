#' Forecast loss measures
#'
#' @param actual Numeric vector of realizations.
#' @param pred Numeric vector of forecasts.
#' @param benchmark Benchmark forecast used for the coefficient of
#'   determination. Default is the prevailing mean of `actual`.
#' @param level Compute the quasi-likelihood loss, which requires both
#'   arguments to be strictly positive. Default `FALSE`.
#' @return A one-row `data.table`.
#' @examples
#' set.seed(1); a <- rnorm(100); f <- a + rnorm(100, sd = 0.5)
#' tpr_metrics(a, f)
#' @export
tpr_metrics <- function(actual, pred, benchmark = NULL, level = FALSE) {
  ok <- is.finite(actual) & is.finite(pred)
  a <- actual[ok]; f <- pred[ok]
  if (is.null(benchmark))
    benchmark <- vapply(seq_along(a),
                        function(i) mean(a[seq_len(max(i - 1L, 1L))]), 0)
  b <- benchmark[ok]
  e <- a - f
  out <- data.table::data.table(
    N = length(e), MSE = mean(e^2), MAE = mean(abs(e)),
    MedAE = stats::median(abs(e)),
    OOS_R2 = 1 - sum(e^2) / sum((a - b)^2))
  if (level) {
    m <- a > 0 & f > 0
    rt <- a[m] / f[m]
    out[, `:=`(QLIKE = mean(rt - log(rt) - 1), N_QLIKE = sum(m),
               N_negative = sum(f <= 0))]
  }
  out[]
}

#' Diebold and Mariano test with a HAC long-run variance
#'
#' A positive statistic means the first loss series is the larger, that
#' is, the second model forecasts better.
#'
#' @param loss_a,loss_b Loss series of equal length.
#' @param lag Truncation lag. `NULL` uses
#'   \eqn{\lfloor 4(n/100)^{2/9}\rfloor}.
#' @return A named vector with the statistic, its p value and the lag.
#' @references
#' Diebold, F. X. and Mariano, R. S. (1995). Comparing predictive
#' accuracy. \emph{Journal of Business and Economic Statistics} 13,
#' 253-263.
#' @examples
#' set.seed(1); la <- rchisq(300, 2); lb <- rchisq(300, 2)
#' tpr_dm(la, lb)
#' @export
tpr_dm <- function(loss_a, loss_b, lag = NULL) {
  d <- loss_a - loss_b; d <- d[is.finite(d)]
  n <- length(d)
  if (n < 20L) return(c(DM = NA_real_, p = NA_real_, lag = NA_real_))
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  dbar <- mean(d); e <- d - dbar
  g0 <- sum(e^2) / n; s <- g0
  if (lag >= 1L) for (l in seq_len(lag)) {
    gl <- sum(e[(l + 1):n] * e[1:(n - l)]) / n
    s <- s + 2 * (1 - l / (lag + 1)) * gl
  }
  if (!is.finite(s) || s <= 0) return(c(DM = NA_real_, p = NA_real_, lag = lag))
  stat <- dbar / sqrt(s / n)
  c(DM = stat, p = 2 * stats::pnorm(-abs(stat)), lag = lag)
}

#' Model confidence set
#'
#' A thin wrapper around `MCS::MCSprocedure` that accepts a matrix of
#' losses and returns a tidy table, extracting the result slot across
#' the versions of that package.
#'
#' @param loss Matrix of losses, one column per model.
#' @param alpha Size of the test. Default `0.10`.
#' @param B Bootstrap replications. Default `5000`.
#' @param statistic Either `"Tmax"` or `"TR"`.
#' @return A `data.table`, or `NULL` if the procedure fails.
#' @examples
#' \donttest{
#' set.seed(1)
#' L <- cbind(a = rchisq(300, 2), b = rchisq(300, 2.2), c = rchisq(300, 3))
#' tpr_mcs(L)
#' }
#' @export
tpr_mcs <- function(loss, alpha = 0.10, B = 5000L, statistic = "Tmax") {
  if (!requireNamespace("MCS", quietly = TRUE))
    stop("Package \"MCS\" is required for tpr_mcs().")
  L <- as.matrix(loss)
  L <- L[stats::complete.cases(L), , drop = FALSE]
  r <- try(MCS::MCSprocedure(Loss = L, alpha = alpha, B = B,
                             statistic = statistic, verbose = FALSE),
           silent = TRUE)
  if (inherits(r, "try-error")) return(NULL)
  tb <- try({
    sl <- intersect(c("show", "Info", "SSM"), methods::slotNames(r))
    if (length(sl)) methods::slot(r, sl[1]) else as.data.frame(r)
  }, silent = TRUE)
  if (inherits(tb, "try-error") || is.null(tb)) return(NULL)
  data.table::as.data.table(as.data.frame(tb), keep.rownames = "model")
}

#' Value-at-Risk backtests
#'
#' Builds \eqn{\widehat{VaR}_{t+1}=z_\alpha\sqrt{\hat f_{t+1}}} from a
#' variance forecast and tests the resulting violation sequence with the
#' unconditional coverage test of Kupiec (1995) and the independence
#' test of Christoffersen (1998).
#'
#' @param variance_forecast Numeric vector of one-step variance
#'   forecasts, strictly positive.
#' @param returns Realized returns for the same days.
#' @param alpha Tail probability, for example `0.01`.
#' @param dist Either `"normal"` or `"student"`.
#' @param df Degrees of freedom when `dist = "student"`. Default `5`.
#' @return A one-row `data.table` with the violation rate and both
#'   tests.
#' @references
#' Kupiec, P. (1995). Techniques for verifying the accuracy of risk
#' measurement models. \emph{Journal of Derivatives} 3, 73-84.
#' Christoffersen, P. (1998). Evaluating interval forecasts.
#' \emph{International Economic Review} 39, 841-862.
#' @examples
#' set.seed(1)
#' v <- rep(4e-4, 1000); r <- rnorm(1000, sd = sqrt(v))
#' tpr_var_backtest(v, r, alpha = 0.05)
#' @export
tpr_var_backtest <- function(variance_forecast, returns, alpha = 0.01,
                             dist = c("normal", "student"), df = 5) {
  dist <- match.arg(dist)
  ok <- is.finite(variance_forecast) & is.finite(returns) &
        variance_forecast > 0
  v <- variance_forecast[ok]; r <- returns[ok]
  z <- if (dist == "normal") stats::qnorm(alpha) else
       stats::qt(alpha, df) * sqrt((df - 2) / df)
  hit <- as.integer(r < z * sqrt(v))
  n <- length(hit); x <- sum(hit)
  kp <- .kupiec(n, x, alpha); ci <- .christoffersen(hit)
  data.table::data.table(
    alpha = alpha, dist = dist, N = n, violations = x,
    rate = 100 * x / n, expected = 100 * alpha,
    kupiec_LR = kp[1], kupiec_p = kp[2],
    ind_LR = ci[1], ind_p = ci[2])
}

.kupiec <- function(n, x, p) {
  if (x == 0L || x == n) return(c(NA_real_, NA_real_))
  ph <- x / n
  lr <- -2 * ((n - x) * log(1 - p) + x * log(p) -
              (n - x) * log(1 - ph) - x * log(ph))
  c(lr, 1 - stats::pchisq(lr, 1))
}

.christoffersen <- function(v) {
  n <- length(v); a <- v[-n]; b <- v[-1]
  n00 <- sum(a == 0 & b == 0); n01 <- sum(a == 0 & b == 1)
  n10 <- sum(a == 1 & b == 0); n11 <- sum(a == 1 & b == 1)
  if ((n00 + n01) == 0 || (n10 + n11) == 0 || (n01 + n11) == 0)
    return(c(NA_real_, NA_real_))
  p01 <- n01 / (n00 + n01); p11 <- n11 / (n10 + n11)
  p <- (n01 + n11) / (n00 + n01 + n10 + n11)
  if (p01 %in% c(0, 1) || p11 %in% c(0, 1) || p %in% c(0, 1))
    return(c(NA_real_, NA_real_))
  lr <- -2 * ((n00 + n10) * log(1 - p) + (n01 + n11) * log(p) -
              n00 * log(1 - p01) - n01 * log(p01) -
              n10 * log(1 - p11) - n11 * log(p11))
  c(lr, 1 - stats::pchisq(lr, 1))
}
