# tpridge

Two-parameter ridge estimation of HAR realized-volatility models.

Ridge regression buys a variance reduction at the cost of in-sample fit.
The two-parameter estimator of Lipovetsky and Conklin (2005) recovers
part of that fit by rescaling the ridge coefficient vector by a scalar
`q`. This package implements the estimator, the analytical results that
characterise it, and the whole pipeline around it: downloading
five-minute prices from Binance, building realized measures, rolling
estimation, forecast evaluation, and the Monte Carlo design of the
accompanying paper.

## Installation

```r
# install.packages("remotes")
remotes::install_github("talhaomer472/tpridge", build_vignettes = TRUE)
```

Installing from a local copy of the sources, or troubleshooting a failed
installation, is covered step by step in `INSTALL.md`.

## A minute of it

```r
library(tpridge)

set.seed(1)
X <- matrix(rnorm(400), ncol = 4)
y <- as.numeric(X %*% c(1, 0.6, 0.3, 0.1)) + rnorm(100)

tpr_lambda(X, y)            # data-driven penalty
tpr_q(X, y, lambda = 20)    # the two-parameter scalar, on its own
fit <- tpr_fit(X, y)        # OLS, ridge-I and Ridge-II together
plot(fit, which = "path")
```

`tpr_lambda()` and `tpr_q()` take any design matrix, so they can be used
outside the HAR setting.

## The whole pipeline

```r
k    <- binance_klines("BTCUSDT", from = "2018-05", to = "2025-12")
rm_  <- realized_measures(k)
feat <- har_features(rm_$RV, date = rm_$date,
                     extra = rm_[, c("BV", "Jump", "RSV_neg")])
des  <- har_design(feat)

roll <- tpr_rolling(des$X, des$y, window = 365, date = des$date)
summary(roll)
plot(roll, which = "forecast")
```

## What the package checks

Four results are proved in the paper and verified in the test suite at
machine precision.

| | |
|---|---|
| `q > 1` for every `lambda > 0` | derived, not assumed |
| fit gain `= (q-1)^2 \|X b_I\|^2 / y'y` | exact closed form |
| `R2(OLS) >= R2(II) >= R2(I)` | no estimator beats OLS in sample |
| `b_II` tends to a finite non-zero limit | ridge-I collapses, Ridge-II does not |

## Function reference

**Estimation** `tpr_fit`, `tpr_q`, `tpr_lambda`, `tpr_eff_df`,
`tpr_window`

**Data** `binance_klines`, `binance_symbols`, `realized_measures`

**HAR** `har_features`, `har_design`, `har_simulate`

**Forecasting** `tpr_rolling`

**Evaluation** `tpr_metrics`, `tpr_dm`, `tpr_mcs`, `tpr_var_backtest`

**Simulation** `tpr_montecarlo`

**Methods** `print`, `summary`, `coef`, `fitted`, `predict` and `plot`
for the fitted, rolling and Monte Carlo objects.

## Documentation

Three layers, in order of how much detail you want.

**The user manual**, `inst/manual/tpridge-manual.pdf`, 18 pages. Written to
be followed from start to finish: installation, the idea in one page,
the estimators on their own, downloading from Binance, building the HAR
model, rolling forecasts, evaluation, the Monte Carlo study, a complete
worked example from download to significance test, a function reference
and a troubleshooting chapter. Every code block runs as printed.

**The vignette**, `vignette("tpridge")`, a shorter tour of the same
ground.

**The reference manual**, generated from the roxygen comments, which
documents every argument of every function:

```r
install.packages(c("roxygen2", "devtools", "rprojroot"))
devtools::document()      # writes man/*.Rd from the roxygen comments
devtools::check()         # R CMD check
devtools::build_manual()  # tpridge_0.1.0.pdf
```

The LaTeX source of the user manual is in `inst/manual/tpridge-manual.tex`
if you want to edit it.

The vignette is the long-form documentation:

```r
vignette("tpridge")
```

## References

Corsi, F. (2009). A simple approximate long-memory model of realized
volatility. *Journal of Financial Econometrics* 7, 174-196.

Hoerl, A. E. and Kennard, R. W. (1970). Ridge regression: biased
estimation for nonorthogonal problems. *Technometrics* 12, 55-67.

Hoerl, A. E., Kennard, R. W. and Baldwin, K. F. (1975). Ridge
regression: some simulations. *Communications in Statistics* 4, 105-123.

Lipovetsky, S. and Conklin, W. M. (2005). Ridge regression in
two-parameter solution. *Applied Stochastic Models in Business and
Industry* 21, 525-540.

## Licence

GPL-3.
