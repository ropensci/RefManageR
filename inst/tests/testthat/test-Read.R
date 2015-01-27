context("ReadBib")
library(RefManageR)

test_that("Read.Bib reads in 494 entries from RJC.bib", {
  file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
  bib <- ReadBib(file.name)
  expect_equal(length(bib), 494L)
})

test_that("Read.Bib Ignores entry but does not stop with invalid author/editor", {
  f <- file.path(system.file("Bib", "badFormat.bib", package = "bibtex"))
  bib <- ReadBib(f)
  expect_true(length(bib) == 1L)
})
