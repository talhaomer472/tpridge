#' HAR features from a daily realized measure
#'
#' Builds the daily, weekly and monthly components of the HAR model as
#' rolling averages ending at \eqn{t-1}, together with the lead of the
#' target, so that each row contains a forecast origin and the value it
#' predicts. The logarithmic transformation is the default because a
#' squared error criterion applied to realized variance in levels is
#' dominated by a handful of days.
#'
#' @param rv Numeric vector of daily realized variance, in time order.
#' @param date Optional vector of dates, carried through to the output.
#' @param lags Averaging windows. Default `c(1, 5, 22)`, the daily,
#'   weekly and monthly components of Corsi (2009).
#' @param log Return the logarithm of each component. Default `TRUE`.
#' @param extra Optional `data.frame` of further predictors, aligned
#'   with `rv`, appended unchanged.
#' @return A `data.table` whose first column `y` is the target for the
#'   next day, followed by the HAR components and any extras.
#' @references
#' Corsi, F. (2009). A simple approximate long-memory model of realized
#' volatility. \emph{Journal of Financial Econometrics} 7, 174-196.
#' @examples
#' set.seed(1)
#' rv <- har_simulate(500, sigma2 = 0.5)
#' d <- har_features(rv)
#' head(d)
#' @export
har_features <- function(rv, date = NULL, lags = c(1, 5, 22), log = TRUE,
                         extra = NULL) {
  rv <- as.numeric(rv)
  tr <- function(z) if (log) { z[!is.finite(z) | z <= 0] <- NA_real_; log(z) } else z
  out <- data.table::data.table(y = tr(data.table::shift(rv, type = "lead")))
  for (L in lags) {
    nm <- if (L == 1L) "RV_d" else sprintf("RV_%d", L)
    v <- if (L == 1L) rv else
      data.table::frollmean(rv, as.integer(L), align = "right")
    data.table::set(out, j = nm, value = tr(v))
  }
  ## data.table::set avoids the shallow-copy warning that `:=` raises on
  ## a table built by data.table() and then modified by [[<-
  if (!is.null(date)) data.table::set(out, j = "date", value = date)
  if (!is.null(extra)) {
    e <- data.table::as.data.table(extra)
    if (nrow(e) != length(rv))
      stop("`extra` must have one row per element of `rv`.")
    out <- cbind(out, e)
  }
  out[]
}

#' Design matrix and response from a feature table
#'
#' @param features Output of [har_features()].
#' @param predictors Character vector of column names to use. Default is
#'   every column other than `y` and `date`.
#' @return A list with `X`, `y` and `date`, complete cases only.
#' @examples
#' set.seed(1)
#' d <- har_features(har_simulate(500, sigma2 = 0.5))
#' des <- har_design(d)
#' dim(des$X)
#' @export
har_design <- function(features, predictors = NULL) {
  f <- data.table::as.data.table(features)
  if (!"y" %in% names(f)) stop("`features` must contain a column `y`.")
  if (is.null(predictors))
    predictors <- setdiff(names(f), c("y", "date"))
  miss <- setdiff(predictors, names(f))
  if (length(miss)) stop("Not found in `features`: ",
                         paste(miss, collapse = ", "))
  keep <- stats::complete.cases(f[, c("y", predictors), with = FALSE])
  list(X = as.matrix(f[keep, predictors, with = FALSE]),
       y = f$y[keep],
       date = if ("date" %in% names(f)) f$date[keep] else NULL)
}

#' Simulate a HAR process
#'
#' Generates \eqn{RV_t=\mu_t\eta_t} with
#' \eqn{\mu_t=\alpha+\beta_d RV_{t-1}+\beta_w \overline{RV}^{(w)}_{t-1}
#' +\beta_m \overline{RV}^{(m)}_{t-1}} and
#' \eqn{\eta_t\sim\mathrm{Gamma}(k,1/k)} with unit mean. Positivity is
#' structural: no truncation or transformation is applied, and the
#' conditional mean is exactly the HAR conditional mean.
#'
#' @param n Number of usable observations returned.
#' @param sigma2 Variance of the multiplicative shock, equal to
#'   \eqn{1/k}.
#' @param alpha Intercept, strictly positive.
#' @param betas The three persistence parameters, non-negative and
#'   summing to less than one.
#' @param burn Burn-in discarded before the returned sample.
#' @return A numeric vector of length `n + 22`, so that
#'   [har_features()] yields exactly `n` usable rows.
#' @examples
#' set.seed(1)
#' rv <- har_simulate(1000, sigma2 = 0.5)
#' c(mean = mean(rv), min = min(rv))
#' @export
har_simulate <- function(n, sigma2, alpha = 0.1, betas = c(0.3, 0.3, 0.3),
                         burn = 300L) {
  bd <- betas[1]; bw <- betas[2]; bm <- betas[3]
  if (sigma2 <= 0) stop("`sigma2` must be positive.")
  if (alpha <= 0) stop("`alpha` must be positive.")
  if (any(betas < 0) || sum(betas) >= 1)
    stop("`betas` must be non-negative and sum to less than one.")
  k <- 1 / sigma2
  total <- burn + n + 22L
  eta <- stats::rgamma(total, shape = k, rate = k)
  rv <- numeric(total)
  rv[1:22] <- alpha / (1 - sum(betas))
  for (t in 23:total) {
    mu <- alpha + bd * rv[t - 1L] +
      bw * mean(rv[(t - 5L):(t - 1L)]) +
      bm * mean(rv[(t - 22L):(t - 1L)])
    rv[t] <- mu * eta[t]
  }
  rv[(burn + 1L):total]
}
