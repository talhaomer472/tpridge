#' Daily realized measures from intraday prices
#'
#' Aggregates intraday closes into the daily measures used by the HAR
#' literature. Returns are differenced over the full series rather than
#' within each day, so a complete day contributes one return per
#' interval including the one spanning midnight. This is the right
#' convention for a market that trades continuously; set
#' `within_day = TRUE` to exclude it, at the cost of one return per day.
#'
#' @param x A table with an `open_time` column of class POSIXct and a
#'   `close` column, as returned by [binance_klines()]. Volume columns
#'   are used when present.
#' @param bars_per_day Expected number of intervals in a full day. For
#'   five-minute data this is 288.
#' @param min_share Days with fewer than this share of the expected
#'   intervals have their measures set to `NA`. Default `0.9`.
#' @param within_day Compute returns within each calendar day only.
#'   Default `FALSE`.
#' @return A `data.table` with one row per day: `date`, `n_5m`,
#'   `complete_day`, `RV`, `BV`, `Jump`, `RSV_pos`, `RSV_neg`, `RQ`,
#'   `RSkew`, `RKurt`, `Parkinson`, `ret_d`, `abs_ret_d`, `close`, and,
#'   when the inputs are present, `log_volume`, `num_trades`,
#'   `taker_buy_share`, `amihud_proxy`.
#' @details
#' Realized variance is \eqn{RV_t=\sum_i r_{t,i}^2}; bipower variation
#' is \eqn{BV_t=\frac{\pi}{2}\frac{n}{n-1}\sum_i |r_{t,i}||r_{t,i-1}|};
#' the jump component is \eqn{\max(RV_t-BV_t,0)}; the semivariances sum
#' to \eqn{RV_t} exactly; and realized quarticity is
#' \eqn{\frac{n}{3}\sum_i r_{t,i}^4}. No logarithm is floored: a
#' non-positive input yields `NA` rather than a large negative number.
#' @examples
#' \donttest{
#' k <- binance_klines("BTCUSDT", "2024-01", "2024-02")
#' rm_ <- realized_measures(k)
#' head(rm_[, .(date, RV, BV, Jump)])
#' }
#' @export
realized_measures <- function(x, bars_per_day = 288L, min_share = 0.9,
                              within_day = FALSE) {
  d <- data.table::as.data.table(x)
  if (!all(c("open_time", "close") %in% names(d)))
    stop("`x` needs `open_time` and `close` columns.")
  data.table::setorder(d, open_time)
  d <- unique(d, by = "open_time")
  d[, date := as.Date(open_time)]
  if (within_day) {
    d[, r5 := c(NA_real_, diff(log(close))), by = date]
  } else {
    d[, r5 := c(NA_real_, diff(log(close)))]
  }
  has <- function(cn) cn %in% names(d)

  daily <- d[is.finite(r5), {
    n <- .N
    RV <- sum(r5^2)
    BV <- if (n > 1L) (pi / 2) * (n / (n - 1)) *
            sum(abs(r5[-1]) * abs(r5[-n])) else NA_real_
    list(n_5m = n, RV = RV, BV = BV, Jump = pmax(RV - BV, 0),
         RSV_pos = sum(r5[r5 > 0]^2), RSV_neg = sum(r5[r5 < 0]^2),
         RQ = (n / 3) * sum(r5^4),
         RSkew = if (RV > 0) sqrt(n) * sum(r5^3) / RV^1.5 else NA_real_,
         RKurt = if (RV > 0) n * sum(r5^4) / RV^2 else NA_real_,
         ret_d = sum(r5), abs_ret_d = abs(sum(r5)),
         close = utils::tail(close, 1L),
         high_d = if (has("high")) max(high, na.rm = TRUE) else NA_real_,
         low_d  = if (has("low"))  min(low,  na.rm = TRUE) else NA_real_,
         vol_d  = if (has("volume")) sum(volume, na.rm = TRUE) else NA_real_,
         qvol_d = if (has("quote_volume"))
                    sum(quote_volume, na.rm = TRUE) else NA_real_,
         ntr_d  = if (has("num_trades"))
                    sum(num_trades, na.rm = TRUE) else NA_real_,
         tbb_d  = if (has("taker_buy_base"))
                    sum(taker_buy_base, na.rm = TRUE) else NA_real_)
  }, by = date]

  daily[, `:=`(
    Parkinson = ifelse(is.finite(low_d) & low_d > 0,
                       (log(high_d) - log(low_d))^2 / (4 * log(2)), NA_real_),
    log_volume = ifelse(is.finite(vol_d) & vol_d > 0, log(vol_d), NA_real_),
    num_trades = ntr_d,
    taker_buy_share = ifelse(is.finite(vol_d) & vol_d > 0,
                             tbb_d / vol_d, NA_real_),
    amihud_proxy = ifelse(is.finite(qvol_d) & qvol_d > 0,
                          abs_ret_d / qvol_d, NA_real_),
    complete_day = n_5m >= bars_per_day)]
  daily[, c("high_d", "low_d", "vol_d", "qvol_d", "ntr_d", "tbb_d") := NULL]

  bad <- daily$n_5m < min_share * bars_per_day
  meas <- c("RV", "BV", "Jump", "RSV_pos", "RSV_neg", "RQ", "RSkew",
            "RKurt", "Parkinson", "ret_d", "abs_ret_d", "close",
            "log_volume", "num_trades", "taker_buy_share", "amihud_proxy")
  if (any(bad)) for (cn in intersect(meas, names(daily)))
    data.table::set(daily, which(bad), cn, NA)
  data.table::setorder(daily, date)
  daily[]
}
