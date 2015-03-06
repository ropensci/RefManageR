context("Author Parsing")
library(bibtex)

test_that("Smith, Jr., John", {
  authors <- "Smith, Jr., John"
  parsed <- bibtex:::ArrangeAuthors(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Smith, Jr., John and Mary {Tyler Moore}", {
  authors <- "Smith, Jr., John and Mary {Tyler Moore}"
  parsed <- bibtex:::ArrangeAuthors(authors)
  expect_match(parsed$family[2], "Tyler Moore")
})

test_that("Smith, Jr., John and {MATLAB, Inc.}", {
  authors <- "Smith, Jr., John and {MATLAB, Inc.}"
  parsed <- bibtex:::ArrangeAuthors(authors)
  expect_match(parsed$family[2], "MATLAB, Inc.")
  expect_equal(length(unlist(parsed$family[1])), 2L)
})

test_that("Smith, John Paul and {MATLAB, Inc.}", {
  authors <- "Smith, John Paul and {MATLAB, Inc.}"
  parsed <- bibtex:::ArrangeAuthors(authors)
  expect_equal(length(unlist(parsed$given[1])), 2L)
})

test_that("{de Gama}, Vasco", {
  authors <- "{de Gama}, Vasco"
  parsed <- bibtex:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Mark von Bommel", {
  authors <- "Mark von Bommel"
  parsed <- bibtex:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("de la Soul, Posdnous", {
  authors <- "de la Soul, Posdnous"
  parsed <- bibtex:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("des White, Jr., Walter", {
  authors <- "des White, Jr., Walter"
  parsed <- bibtex:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 3L)
})

test_that("{Herm{\\`e}s International S.A.} and Katzfu{\\ss}, Matthias", {
  authors <- "{Herm{\\`e}s International S.A.} and Katzfu{\\ss}, Matthias"
  parsed <- bibtex:::ArrangeAuthors(authors)
  ## Be careful about locales
  ## expect_match(parsed$family[[1]], "Hermès International S.A.")
  expect_true(grepl(parsed$family[[1]], "Hermès International S.A.", useBytes = TRUE))
  ## expect_match(parsed$family[[2]], "Katzfuß")
  expect_true(grepl(parsed$family[[2]], "Katzfuß", useBytes = TRUE))
})
