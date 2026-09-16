# Installing tpridge, step by step

Follow these in order. Each block is meant to be pasted into the R
console as it stands. Stop and read if a block errors.

## Step 0. Prerequisites

```r
install.packages(c("data.table", "roxygen2", "devtools",
                   "rprojroot", "testthat"))
```

Rtools is NOT needed. This package contains no compiled code. Windows
will warn about it during installation; ignore the warning.

## Step 1. Put the package somewhere and point R at it

```r
base <- "C:/Previous computer data/Research papers/Kristofer, Ibrar, Talha 2023/Computation and simulations"
setwd(base)
unzip("tpridge_for_github.zip", exdir = base)
setwd(file.path(base, "tpridge"))
file.exists("DESCRIPTION")     # must be TRUE
```

Forward slashes, or doubled backslashes. R rejects single backslashes.

## Step 2. Clear anything left from an earlier attempt

This matters. A half-written installation causes a "lazy-load database
is corrupt" error that looks like a package fault but is not.

```r
try(remove.packages("tpridge"), silent = TRUE)
unlink(file.path(.libPaths()[1], "tpridge"), recursive = TRUE, force = TRUE)
unlink(file.path(.libPaths()[1], "00LOCK-tpridge"), recursive = TRUE,
       force = TRUE)
```

Now restart R: Session -> Restart R, or Ctrl+Shift+F10. Windows will not
let R overwrite files in a package that is currently loaded, which is
what corrupts the database.

## Step 3. Generate the help files

```r
setwd(file.path(base, "tpridge"))
roxygen2::roxygenise(clean = TRUE)
length(list.files("man"))      # should be 28
```

The message "Skipping NAMESPACE, it already exists and was not generated
by roxygen2" is expected. NAMESPACE is written by hand so the package
installs without roxygen.

## Step 4. Install

```r
devtools::install(".", args = "--no-byte-compile", upgrade = "never")
```

`--no-byte-compile` avoids a compression bug in R 4.6 on Windows that
produces the corrupt-database error. The package runs at the same speed
either way for everything in it.

Restart R again.

## Step 5. Confirm it works

```r
rm(list = ls())
library(tpridge)

set.seed(1)
X <- matrix(rnorm(400), ncol = 4)
y <- as.numeric(X %*% c(1, 0.6, 0.3, 0.1)) + rnorm(100)
tpr_fit(X, y)
```

Look for the line reading "Proposition 2 residual". It should be around
1e-16. That is the package verifying its own central identity; if it is
not tiny, stop and do not use the results.

`rm(list = ls())` is not optional if you have ever sourced the
replication scripts in this session: they define their own `tpr_fit`,
which would shadow the package one and give
"'arg' must be NULL or a character vector".

## Step 6. Run the tests

```r
testthat::test_local(".")
```

Sixteen tests, including all four propositions. This is the check that
matters; R CMD check adds only documentation-style notes on top.

## If installation still fails

Build a tarball and install from that, bypassing devtools:

```r
options(pkgbuild.check_build_tools = FALSE)
pkgbuild::build(".", dest_path = ".", vignettes = FALSE)
install.packages("tpridge_0.1.0.tar.gz", repos = NULL, type = "source",
                 INSTALL_opts = "--no-byte-compile")
```

## Known messages that are not problems

| Message | Why |
|---|---|
| Rtools is required but not installed | No compiled code here. Ignore. |
| Skipping NAMESPACE | Written by hand, deliberately. |
| lazy-load database is corrupt | Stale or half-written install. Step 2, then restart R. |
| 'arg' must be NULL or a character vector | An old `tpr_fit` in your workspace. `rm(list = ls())`. |
| reached elapsed time limit, rugarch | An optional package being probed. Harmless. |
| Files in vignettes but none in inst/doc | Only when vignette building is skipped. |

## Where to go next

`inst/manual/tpridge-manual.pdf`, eighteen pages, written to be followed
from start to finish. Chapter 3 is the first with runnable code;
Chapter 9 is a complete worked example from downloading prices to a
significance test.
