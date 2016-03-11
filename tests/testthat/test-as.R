context("as.BibEntry")

test_that("list to BibEntry", {
    bib.l <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
      c(bibtype = "article", key = "mclean2014b", title = "My Newer Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
    bib <- as.BibEntry(bib.l)
    expect_is(bib, "BibEntry")
    expect_equal(length(bib), 2L)

    bib <- as.BibEntry(bib.l[1])
    expect_is(bib, "BibEntry")
    expect_equal(length(bib), 1L)
})

test_that("citation to BibEntry", {
    bib <- as.BibEntry(citation())
    expect_is(bib, "BibEntry")
    expect_true(nzchar(bib$key))
})

test_that("is",
   expect_true(is.BibEntry(as.BibEntry(citation())))
)

test_that("unlisted to BibEntry", {
    bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
      c(bibtype = "article", key = "mclean2014b", title = "My Newer Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
    bib <- unlist(as.BibEntry(bib))
    bib <- as.BibEntry(bib)
    expect_is(bib, "BibEntry")
    expect_equal(length(bib), 2L)
})

test_that("data.frame-BibEntry conversion", {
   bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
     author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01-01"),
     c(bibtype = "article", key = "mclean2014b", volume = 10, title = "My Newer Article",
     author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014"))
   bib <- as.BibEntry(bib)
   bib.df <- as.data.frame(bib)
   expect_is(bib.df, "data.frame")
   expect_identical(rownames(bib.df), c("mclean2014a", "mclean2014b"))
   expect_identical(colnames(bib.df), c("bibtype", "title", "author", "journaltitle", "date",
                                        "volume"))
   bib <- as.BibEntry(bib.df)
   expect_is(bib, "BibEntry")
   expect_equal(length(bib), 2L)
})

test_that("BibEntry to data.frame when multiple authors", {
   bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
                 author = "McLean, Mathew W. and Ruppert, David", journaltitle = "The Journal",
                 date = "2014-01-01"))
   bib <- as.BibEntry(bib)
   bib.df <- as.data.frame(bib)
   expect_is(bib.df, "data.frame")
   expect_equal(length(bib), 1L)
})


test_that("character to BibEntry", {
    bib.c <- c(bibtype = "misc", author = "Mathew W. McLean", title = "A title",
             year = 2012, key = "testkey")
    bib <- as.BibEntry(bib.c)
    expect_is(bib, "BibEntry")
    expect_error(as.BibEntry(bib.c[-1]), "Object of class character")  # missing bibtype
    expect_error(as.BibEntry(bib.c[-5]), "Object of class character")  # missing key
})

test_that("bibentry to BibEntry", {
    bib.be <- bibentry("manual", author = "Bob Smith", title = "A manual", year = 2012)
    bib <- as.BibEntry(bib.be)
    expect_is(bib, "BibEntry")
})

test_that("unclass'ed BibEntry to BibEntry", {
    bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
      c(bibtype = "article", key = "mclean2014b", title = "My Newer Article",
      author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
    bib.u <- unclass(as.BibEntry(bib))
    bib <- as.BibEntry(bib.u)
    expect_is(bib, "BibEntry")
    expect_equal(length(bib), 2L)

    bib <- as.BibEntry(bib.u[1])
    expect_is(bib, "BibEntry")
    expect_equal(length(bib), 1L)
})
