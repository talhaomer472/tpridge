## data.table uses non-standard evaluation, which R CMD check reports as
## undefined global variables. Declaring them here silences the note
## without weakening the check elsewhere.
utils::globalVariables(c(
  ".", ".N", ".SD", "open_time", "close", "date", "r5", "high", "low",
  "volume", "quote_volume", "num_trades", "taker_buy_base", "n_5m",
  "RV", "BV", "Jump", "RSV_pos", "RSV_neg", "RQ", "RSkew", "RKurt",
  "Parkinson", "ret_d", "abs_ret_d", "high_d", "low_d", "vol_d",
  "qvol_d", "ntr_d", "tbb_d", "complete_day", "log_volume",
  "taker_buy_share", "amihud_proxy", "actual", "pred", "prev_mean",
  "r2_in", "lambda", "q", "eff_df", "estimator", "rule", "target_df",
  "origin", "gap_empirical", "gap_theoretical", "sigma2", "n", "W",
  "actual_level", "pred_level", "q_min", "r2_oos", "msfe", "rho"))
