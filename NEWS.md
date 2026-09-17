# tpridge 0.2.0

## New

* `tpr_lm(formula, data)` applies the estimator to any regression
  through a formula interface, with `summary()`, `predict()` and
  `residuals()` methods. The two-parameter ridge is a general remedy
  for an ill-conditioned design; nothing about it is specific to
  volatility models.

* `summary.tpr_lm()` reports the three coefficient vectors side by side
  together with variance inflation factors, the condition number of the
  design, the penalty, `q` and the effective degrees of freedom. No
  standard errors are given, and the output says why: the sampling
  distribution of a shrinkage estimator is not that of least squares.

* A second vignette, `vignette("tpridge-regression")`, works through the
  Longley data, the design Hoerl and Kennard used when they introduced
  ridge regression.

* The user manual gains a chapter on general regression and an expanded
  troubleshooting chapter covering the installation messages Windows
  users meet. Twenty-one pages, at `inst/manual/tpridge-manual.pdf`.

* A reference manual in the standard R format, one entry per function
  with description, usage, arguments, value and examples. Thirty-six
  pages, at `inst/manual/tpridge-reference-manual.pdf`.

## Fixed

* `har_features()` no longer raises a shallow-copy warning from
  `data.table` when a `date` column is supplied.

# tpridge 0.1.0

First release. Estimation, Binance download, realized measures, rolling
forecasts, forecast evaluation and the Monte Carlo design of the
accompanying paper.
