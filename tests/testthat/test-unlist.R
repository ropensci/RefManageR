context("Un and re-listing")

bib.l <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
  author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
  c(bibtype = "article", key = "mclean2014b", title = "My Newer Article",
  author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
bib <- as.BibEntry(bib.l)

test_that("Updating field name", {
    bib <- UpdateFieldName(bib, "journaltitle", "journal")
    expect_identical(levels(bib)[[1]], c("title", "author", "journal", "date"))
})

bib.ul <- unlist(bib)

test_that("unlist", {
  expect_is(bib.ul, "list")
  expect_true(all(c("dateobj", "key", "bibtype") %in% names(bib.ul)))
})

test_that("relist", {
   expect_is(RelistBibEntry(bib.ul), "BibEntry")
})
