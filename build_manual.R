## Regenerate the documentation, check the package, build the manual.
## Run from the package root.
for (p in c("roxygen2", "devtools", "rprojroot", "data.table", "testthat"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
## rprojroot is what roxygen2 uses to find the package root. Without it
## the error reads "could not find a root DESCRIPTION file", which is
## misleading: the file is there, the helper package is not.
roxygen2::roxygenise(clean = TRUE)   # writes man/*.Rd
devtools::check()                    # R CMD check, including the tests
devtools::build_manual(path = ".")   # tpridge_0.1.0.pdf
devtools::build_vignettes()
