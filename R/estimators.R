#' @keywords internal
#' @aliases tpridge-package
"_PACKAGE"

## ---------------------------------------------------------------------
##  Core estimators. Every function here is pure: it takes a design
##  matrix and a response and returns a value. Nothing is read from the
##  global environment, so they can be used on any regression problem,
##  not only on HAR models.
## ---------------------------------------------------------------------

#' Effective degrees of freedom of the ridge operator
#'
#' The quantity \eqn{\mathrm{df}(\lambda)=\sum_j e_j/(e_j+\lambda)},
#' where \eqn{e_j} are the eigenvalues of \eqn{X'X}. It falls from
#' \eqn{p} at \eqn{\lambda=0} towards zero as the penalty grows, and is
#' comparable across sample sizes and designs in a way that \eqn{\lambda}
#' itself is not.
#'
#' @param x Either a numeric matrix of regressors, or a vector of
#'   eigenvalues of the cross-product matrix.
#' @param lambda Non-negative penalty.
#' @param standardize Logical. If `x` is a matrix, standardize its
#'   columns before forming the cross-product. Default `TRUE`.
#' @return A single number, the effective degrees of freedom.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' tpr_eff_df(X, lambda = 0)
#' tpr_eff_df(X, lambda = 1e6)
#' @export
tpr_eff_df <- function(x, lambda, standardize = TRUE) {
  ev <- .eigenvalues(x, standardize)
  if (!is.numeric(lambda) || length(lambda) != 1L || lambda < 0)
    stop("`lambda` must be a single non-negative number.")
  sum(ev / (ev + lambda))
}

#' Ridge penalty
#'
#' Two selection rules. `"hkb"` is the data-driven rule of Hoerl,
#' Kennard and Baldwin (1975),
#' \eqn{\hat\lambda = p\hat\sigma^2/\hat\beta'_{OLS}\hat\beta_{OLS}}.
#' `"df"` solves \eqn{\mathrm{df}(\lambda)=} `target_df` by
#' one-dimensional root finding, which lets the user state the amount of
#' shrinkage in interpretable units.
#'
#' @param X Numeric matrix of regressors, without an intercept column.
#' @param y Numeric response.
#' @param rule Either `"hkb"` or `"df"`.
#' @param target_df Target effective degrees of freedom, required when
#'   `rule = "df"`. Must lie in `(0, ncol(X)]`.
#' @param standardize Logical. Standardize the columns of `X` and centre
#'   `y` first. Default `TRUE`, and recommended: the penalty is not
#'   scale invariant.
#' @return A single non-negative number.
#' @references
#' Hoerl, A. E., Kennard, R. W. and Baldwin, K. F. (1975). Ridge
#' regression: some simulations. \emph{Communications in Statistics}
#' 4, 105-123.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' y <- as.numeric(X %*% c(1, 0.5, 0.2)) + rnorm(100)
#' tpr_lambda(X, y)
#' tpr_lambda(X, y, rule = "df", target_df = 1.5)
#' @export
tpr_lambda <- function(X, y, rule = c("hkb", "df"), target_df = NULL,
                       standardize = TRUE) {
  rule <- match.arg(rule)
  w <- tpr_window(X, y, standardize = standardize)
  if (rule == "hkb") return(.lambda_hkb(w))
  if (is.null(target_df)) stop("`target_df` is required when rule = \"df\".")
  .lambda_for_df(w$eigenvalues, target_df)
}

#' The two-parameter scalar q
#'
#' The scalar that maximises the coefficient of determination along the
#' ray \eqn{\{q\hat\beta_{I}\}}, namely \eqn{q = Q_1/Q_2} with
#' \eqn{Q_1 = \hat\beta_I' r} and \eqn{Q_2 = \hat\beta_I' G \hat\beta_I}.
#' It exceeds one for every \eqn{\lambda > 0}, since
#' \eqn{q = 1 + \lambda\|\hat\beta_I\|^2/\|X\hat\beta_I\|^2}.
#'
#' @inheritParams tpr_lambda
#' @param lambda Penalty. If `NULL` it is selected by `rule`.
#' @return A single number greater than one, carrying attributes
#'   `lambda`, `rho` and `eff_df`.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' y <- as.numeric(X %*% c(1, 0.5, 0.2)) + rnorm(100)
#' q <- tpr_q(X, y, lambda = 10)
#' q > 1
#' attr(q, "eff_df")
#' @export
tpr_q <- function(X, y, lambda = NULL, rule = c("hkb", "df"),
                  target_df = NULL, standardize = TRUE) {
  rule <- match.arg(rule)
  w <- tpr_window(X, y, standardize = standardize)
  if (is.null(lambda))
    lambda <- if (rule == "hkb") .lambda_hkb(w) else
              .lambda_for_df(w$eigenvalues, target_df)
  z <- .ridge_solve(w, lambda)
  structure(z$q, lambda = lambda, rho = z$rho,
            eff_df = tpr_eff_df(w$eigenvalues, lambda))
}

#' Fit OLS, one-parameter ridge and two-parameter ridge
#'
#' All three estimators come from a single decomposition of the design,
#' so the comparison between them is exact. The same \eqn{\lambda}
#' enters the one-parameter estimator and the computation of \eqn{q},
#' which is required for \eqn{q} to maximise the fit along the ray.
#'
#' @inheritParams tpr_q
#' @return An object of class `tpr_fit`.
#' @references
#' Lipovetsky, S. and Conklin, W. M. (2005). Ridge regression in
#' two-parameter solution. \emph{Applied Stochastic Models in Business
#' and Industry} 21, 525-540.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' y <- as.numeric(X %*% c(1, 0.5, 0.2)) + rnorm(100)
#' f <- tpr_fit(X, y)
#' f
#' coef(f, "Ridge-II")
#' abs(f$gap_empirical - f$gap_theoretical) < 1e-12
#' @export
tpr_fit <- function(X, y, lambda = NULL, rule = c("hkb", "df"),
                    target_df = NULL, standardize = TRUE) {
  rule <- match.arg(rule)
  w <- tpr_window(X, y, standardize = standardize)
  if (is.null(lambda))
    lambda <- if (rule == "hkb") .lambda_hkb(w) else
              .lambda_for_df(w$eigenvalues, target_df)
  if (!is.finite(lambda) || lambda < 0) lambda <- 0
  z <- .ridge_solve(w, lambda)

  b <- list(OLS = w$b_ols, `Ridge-I` = z$b_I, `Ridge-II` = z$b_II)
  r2 <- vapply(b, function(bb) 1 - sum((w$yc - w$Z %*% bb)^2) / w$tss, 0)
  orig <- lapply(b, function(bb) {
    bo <- bb / w$scale
    stats::setNames(c(w$center_y - sum(bo * w$center_x), bo),
                    c("(Intercept)", colnames(w$Z)))
  })
  structure(list(
    coefficients = orig, coefficients_std = b, r2_in = r2,
    lambda = lambda, q = z$q, rho = z$rho,
    eff_df = tpr_eff_df(w$eigenvalues, lambda),
    condition = max(w$eigenvalues) / min(w$eigenvalues),
    eigen_min = min(w$eigenvalues),
    gap_empirical = unname(r2[["Ridge-II"]] - r2[["Ridge-I"]]),
    gap_theoretical = (z$q - 1)^2 * z$Q2 / w$tss,
    recovery = if (r2[["OLS"]] - r2[["Ridge-I"]] > 1e-14)
      unname((r2[["Ridge-II"]] - r2[["Ridge-I"]]) /
             (r2[["OLS"]] - r2[["Ridge-I"]])) else NA_real_,
    n = w$n, p = w$p, sigma2 = w$sigma2, window = w),
    class = "tpr_fit")
}

#' Decompose a design once
#'
#' Standardizes the regressors, centres the response, and returns the
#' sufficient statistics together with the eigenvalues, so that every
#' penalty rule can be evaluated without repeating the expensive work.
#'
#' @inheritParams tpr_lambda
#' @return A list of sufficient statistics.
#' @examples
#' set.seed(1)
#' w <- tpr_window(matrix(rnorm(300), ncol = 3), rnorm(100))
#' round(w$eigenvalues, 2)
#' @export
tpr_window <- function(X, y, standardize = TRUE) {
  X <- as.matrix(X)
  if (!is.numeric(X)) stop("`X` must be numeric.")
  if (length(y) != nrow(X)) stop("`y` must have one element per row of `X`.")
  keep <- stats::complete.cases(X, y)
  if (sum(keep) < ncol(X) + 2L)
    stop("Too few complete observations to estimate the model.")
  X <- X[keep, , drop = FALSE]; y <- as.numeric(y)[keep]
  n <- nrow(X); p <- ncol(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(p))

  ctr <- colMeans(X)
  Z <- X - rep(ctr, each = n)
  sdv <- if (standardize) sqrt(colMeans(Z * Z)) else rep(1, p)
  sdv[!is.finite(sdv) | sdv <= 0] <- 1
  Z <- Z / rep(sdv, each = n)
  colnames(Z) <- colnames(X)

  ybar <- mean(y); yc <- y - ybar
  G <- crossprod(Z); r <- as.numeric(crossprod(Z, yc))
  ev <- eigen(G, symmetric = TRUE, only.values = TRUE)$values
  b_ols <- tryCatch(as.numeric(solve(G, r)),
                    error = function(e) as.numeric(qr.solve(G, r)))
  res <- as.numeric(yc - Z %*% b_ols)
  rss <- sum(res^2)
  list(Z = Z, G = G, r = r, yc = yc, eigenvalues = ev, b_ols = b_ols,
       rss_ols = rss, sigma2 = rss / max(n - p - 1L, 1L), tss = sum(yc^2),
       center_x = ctr, scale = sdv, center_y = ybar, n = n, p = p)
}

## ---- internals ------------------------------------------------------

.eigenvalues <- function(x, standardize = TRUE) {
  if (is.matrix(x) || is.data.frame(x)) {
    x <- as.matrix(x); n <- nrow(x)
    Z <- x - rep(colMeans(x), each = n)
    if (standardize) {
      s <- sqrt(colMeans(Z * Z)); s[!is.finite(s) | s <= 0] <- 1
      Z <- Z / rep(s, each = n)
    }
    eigen(crossprod(Z), symmetric = TRUE, only.values = TRUE)$values
  } else {
    ev <- as.numeric(x)
    if (any(ev <= 0)) stop("Eigenvalues must be strictly positive.")
    ev
  }
}

.lambda_hkb <- function(w) {
  d <- sum(w$b_ols^2)
  if (!is.finite(d) || d <= 0) return(0)
  as.numeric(w$p * w$sigma2 / d)
}

.lambda_for_df <- function(ev, target) {
  p <- length(ev)
  if (is.null(target)) stop("`target_df` is required.")
  if (target <= 0 || target > p) stop("`target_df` must lie in (0, p].")
  if (isTRUE(all.equal(target, p))) return(0)
  f <- function(l) sum(ev / (ev + l)) - target
  hi <- max(ev) * 1e6
  while (f(hi) > 0 && hi < 1e18) hi <- hi * 100
  stats::uniroot(f, c(1e-10, hi), tol = 1e-10)$root
}

.ridge_solve <- function(w, lambda) {
  A <- w$G; diag(A) <- diag(A) + lambda
  b_I <- tryCatch(as.numeric(solve(A, w$r)),
                  error = function(e) as.numeric(qr.solve(A, w$r)))
  Q1 <- sum(b_I * w$r)
  Q2 <- as.numeric(crossprod(b_I, w$G %*% b_I))
  q <- if (is.finite(Q2) && Q2 > .Machine$double.eps) Q1 / Q2 else 1
  if (!is.finite(q)) q <- 1
  list(b_I = b_I, b_II = q * b_I, q = q, Q1 = Q1, Q2 = Q2,
       rho = if (sum(b_I^2) > 0) Q2 / sum(b_I^2) else NA_real_)
}
