context("ReadBib")

test_that("Read.Bib Ignores entry but does not stop with invalid author/editor", {
  f <- file.path(system.file("Bib", "badFormat.bib", package = "bibtex"))
  bib <- read.bib(f)
  expect_true(length(bib) == 1L)
})
