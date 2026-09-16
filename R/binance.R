#' Download five-minute klines from Binance
#'
#' Downloads the monthly kline archives published at
#' `data.binance.vision` and returns them as a single table. Nothing is
#' required beyond an internet connection: the files are public and no
#' API key is needed.
#'
#' Two details matter for replication and are handled here. Binance
#' switched the `open_time` field from milliseconds to microseconds
#' partway through the history, which is detected and normalised. And
#' some archives carry a header row while others do not, which is also
#' detected.
#'
#' @param symbol Trading pair, for example `"BTCUSDT"`.
#' @param from,to First and last month, as `"YYYY-MM"`.
#' @param interval Kline interval. Default `"5m"`.
#' @param dir Directory in which to cache the downloaded archives. A
#'   month already present is not downloaded again. Default is a
#'   session temporary directory.
#' @param quiet Suppress progress messages.
#' @return A `data.table` with columns `open_time` (POSIXct, UTC),
#'   `open`, `high`, `low`, `close`, `volume`, `quote_volume`,
#'   `num_trades`, `taker_buy_base`, `taker_buy_quote`.
#' @examples
#' \donttest{
#' # one month of Bitcoin data
#' k <- binance_klines("BTCUSDT", from = "2024-01", to = "2024-01")
#' head(k)
#' }
#' @export
binance_klines <- function(symbol, from, to, interval = "5m",
                           dir = file.path(tempdir(), "binance"),
                           quiet = FALSE) {
  stopifnot(is.character(symbol), length(symbol) == 1L)
  months <- .month_seq(from, to)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base <- "https://data.binance.vision/data/spot/monthly/klines"

  out <- vector("list", length(months))
  for (i in seq_along(months)) {
    m  <- months[i]
    fn <- sprintf("%s-%s-%s.zip", symbol, interval, m)
    dest <- file.path(dir, fn)
    if (!file.exists(dest)) {
      url <- sprintf("%s/%s/%s/%s", base, symbol, interval, fn)
      ok <- tryCatch({
        utils::download.file(url, dest, mode = "wb", quiet = TRUE); TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) {
        if (!quiet) message("  not available: ", fn)
        if (file.exists(dest)) unlink(dest)
        next
      }
    }
    out[[i]] <- .read_kline_zip(dest)
    if (!quiet) message(sprintf("  %s  %s rows", m,
                        format(nrow(out[[i]]), big.mark = ",")))
  }
  k <- data.table::rbindlist(Filter(Negate(is.null), out), fill = TRUE)
  if (!nrow(k)) stop("No data downloaded. Check the symbol and the dates.")
  data.table::setorder(k, open_time)
  unique(k, by = "open_time")
}

#' Symbols used in the accompanying paper
#'
#' @return A character vector of Binance trading pairs.
#' @examples
#' binance_symbols()
#' @export
binance_symbols <- function()
  c("BTCUSDT", "ETHUSDT", "LTCUSDT", "XRPUSDT", "BNBUSDT")

## ---- internals ------------------------------------------------------

.month_seq <- function(from, to) {
  f <- as.Date(paste0(from, "-01")); t <- as.Date(paste0(to, "-01"))
  if (is.na(f) || is.na(t)) stop("`from` and `to` must look like \"2024-01\".")
  if (t < f) stop("`to` must not precede `from`.")
  s <- seq(f, t, by = "month")
  format(s, "%Y-%m")
}

.read_kline_zip <- function(path) {
  nm <- c("open_time", "open", "high", "low", "close", "volume",
          "close_time", "quote_volume", "num_trades",
          "taker_buy_base", "taker_buy_quote", "ignore")
  inner <- utils::unzip(path, list = TRUE)$Name[1]
  tmp <- utils::unzip(path, files = inner, exdir = tempdir())
  d <- data.table::fread(tmp, header = FALSE, showProgress = FALSE)
  unlink(tmp)
  if (is.character(d[[1]])) {                 # some archives carry a header
    d <- d[!grepl("open_time", d[[1]], ignore.case = TRUE)]
    for (j in seq_len(ncol(d)))
      data.table::set(d, j = j, value = as.numeric(d[[j]]))
  }
  d <- d[, seq_len(min(12L, ncol(d))), with = FALSE]
  data.table::setnames(d, nm[seq_len(ncol(d))])
  d[, open_time := as.numeric(open_time)]
  ## milliseconds became microseconds partway through the history
  d[open_time > 1e14, open_time := open_time / 1000]
  d[, open_time := as.POSIXct(open_time / 1000, origin = "1970-01-01",
                              tz = "UTC")]
  d[, c("close_time", "ignore") := NULL]
  d[]
}
