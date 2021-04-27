context("ReadBib")

## unloadNamespace("RefManageR")
## library(RefManageR)
skip_if_no_parser <- function() {
  have_bp <- py_module_available("bibtexparser")
  if (!have_bp)
    skip("bibtexparser not available for testing")
}

test_that("ReadBib reads in 494 entries from RJC.bib", {
  skip_if_no_parser()
  file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
  bib <- ReadBib(file.name)
  expect_equal(length(bib), 494L)
})

test_that("ReadBib ignores entry but does not stop if invalid author/editor", {
  skip_if_no_parser()
  f <- file.path(system.file("Bib", "badFormat.bib", package = "RefManageR"))
  bib <- ReadBib(f, check = "error")
  expect_true(length(bib) == 1L)
})

test_that("ReadBib reads in 92 entries from biblatexExamples.bib", {
    skip_if_not_installed("bibtex")
    file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
    expect_warning(
        bib <- ReadBib(file.name, check = "warn", use.bibtex = TRUE),
        "has to specify the field: author OR editor")
  expect_equal(length(bib), 92)
})
