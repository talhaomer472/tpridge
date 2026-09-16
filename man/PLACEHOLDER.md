This folder will hold the generated .Rd help files.

Run once, from the package root:

    roxygen2::roxygenise(clean = TRUE)

That writes 28 .Rd files here from the roxygen comments in R/*.R, and
deletes this placeholder. Commit the generated files: they let anyone
install the package without roxygen2.
