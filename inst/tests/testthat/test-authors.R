context("Author Parsing")
library(RefManageR)

test_that("Smith, Jr., John", {
  authors <- "Smith, Jr., John"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Smith, Jr., John and Mary {Tyler Moore}", {
  authors <- "Smith, Jr., John and Mary {Tyler Moore}"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_match(parsed$family[2], "Tyler Moore")
})

test_that("Smith, Jr., John and {MATLAB, Inc.}", {
  authors <- "Smith, Jr., John and {MATLAB, Inc.}"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_match(parsed$family[2], "MATLAB, Inc.")
  expect_equal(length(unlist(parsed$family[1])), 2L)
})

test_that("Smith, John Paul and {MATLAB, Inc.}", {
  authors <- "Smith, John Paul and {MATLAB, Inc.}"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_equal(length(unlist(parsed$given[1])), 2L)
})

test_that("{de Gama}, Vasco", {
  authors <- "{de Gama}, Vasco"
  parsed <- RefManageR:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Mark von Bommel", {
  authors <- "Mark von Bommel"
  parsed <- RefManageR:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("de la Soul, Posdnous", {
  authors <- "de la Soul, Posdnous"
  parsed <- RefManageR:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("des White, Jr., Walter", {
  authors <- "des White, Jr., Walter"
  parsed <- RefManageR:::ArrangeSingleAuthor(authors)
  expect_equal(length(parsed$family), 3L)
})

