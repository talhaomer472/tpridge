# Building the documentation

The `.Rd` help files are generated from the roxygen comments in `R/*.R`.
They are not shipped pre-generated so that the sources stay the single
place where the documentation lives.

## First run

```r
install.packages(c("roxygen2", "devtools", "rprojroot", "data.table",
                   "testthat"))

setwd("path/to/tpridge")
roxygen2::roxygenise(clean = TRUE)   # creates man/*.Rd
devtools::install(".")               # installs the package
devtools::check()                    # R CMD check, runs the tests
```

`rprojroot` is needed by roxygen2 to locate the package root. Without it
you get a misleading error saying no `DESCRIPTION` file could be found,
even when one is plainly there.

Rtools is not required. This package contains no compiled code, so the
warning about it during the build can be ignored on Windows.

## Reference manual as a PDF

```r
devtools::build_manual(path = ".")   # tpridge_0.1.0.pdf
```

## The user manual

Already built, at `inst/manual/tpridge-manual.pdf`, with its LaTeX source
beside it. It is the document to read first; the reference manual above
is for looking up arguments.
