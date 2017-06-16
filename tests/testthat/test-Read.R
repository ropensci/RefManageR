context("ReadBib")

## unloadNamespace("RefManageR")
## library(RefManageR)

test_that("ReadBib reads in 494 entries from RJC.bib", {
  file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
  bib <- ReadBib(file.name)
  expect_equal(length(bib), 494L)
})

test_that("ReadBib ignores entry but does not stop if invalid author/editor", {
  f <- file.path(system.file("Bib", "badFormat.bib", package = "RefManageR"))
  bib <- ReadBib(f, check = "error")
  expect_true(length(bib) == 1L)
})
