context("Author Parsing")


test_that("Smith, Jr., John", {
  authors <- "Smith, Jr., John"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  expect_equal(length(parsed$family), 2L)
})

test_that("Smith, Jr., John and Mary {Tyler Moore}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Smith, Jr., John and Mary {Tyler Moore}",
                  journaltitle = "The Journal Title", date = "2014-02-06",
                  pubstate = "forthcoming")
  expect_match(bib$author[[2]]$family, "Tyler Moore")
})

test_that("Smith, Jr., John and {MATLAB, Inc.}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Smith, Jr., John and {MATLAB, Inc.}",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_match(bib$author[[2]]$family, "MATLAB, Inc.")
  expect_equal(length(bib$author[[1]]$family), 2L)
})

test_that("Smith, John Paul and {MATLAB, Inc.}", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Smith, John Paul and {MATLAB, Inc.}",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_equal(length(bib$author$given[[1]]), 2L)
})

test_that("{de Gama}, Vasco", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title", author = "{de Gama}, Vasco",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_equal(length(bib$author[[1]]$family), 2L)
})

test_that("Mark von Bommel", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "Mark von Bommel",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_equal(length(bib$author[[1]]$family), 2L)
})

test_that("de la Soul, Posdnous", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title",
                  author = "de la Soul, Posdnous",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_equal(length(bib$author[[1]]$family), 2L)
})

test_that("des White, Jr., Walter", {
  bib <- BibEntry(bibtype = "Article", key = "mclean2014",
                  title = "An Article Title", author = "des White, Jr., Walter",
                  journaltitle = "The Journal Title", date = "2014-02-06")
  expect_equal(length(bib$author[[1]]$family), 3L)
})

test_that("{Herm{\\`e}s International S.A.} and Katzfu{\\ss}, Matthias", {
  authors <- "{Herm{\\`e}s International S.A.} and Katzfu{\\ss}, Matthias"
  parsed <- RefManageR:::ArrangeAuthors(authors)
  ## Be careful about locales
  ## expect_match(parsed$family[[1]], "Hermès International S.A.")
  expect_true(grepl(parsed$family[[1]], "Hermès International S.A.",
                    useBytes = TRUE))
  ## expect_match(parsed$family[[2]], "Katzfuß")
  expect_true(grepl(parsed$family[[2]], "Katzfuß", useBytes = TRUE))
})
