context("Bibentry methods")

expect_length <- function(obj, len){
    expect_equal(length(obj), len)
}

bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                           package = "RefManageR"), check = FALSE)


test_that("addition operator", {
    res <- bib[1:3] + bib[3:4]
    expect_length(res, 4L)
})

test_that("addition operator, different merge.fields.to.check", {
    BibOptions(merge.fields.to.check = c("key", "author"))
    res <- bib[1:40] + bib[41:length(bib)]
    expect_length(res, length(bib))
    BibOptions(merge.fields.to.check = c("author"))
    res <- bib[author = "aristotle"][1:2] + bib[author = "aristotle"][3:4]
    expect_length(res, 2L)
    BibOptions(merge.fields.to.check = "key")
})


test_that("merge function, two fields to check", {
    bib1 <- bib[author = "knuth"][1:3]
    bib2 <- bib[author = "knuth"][4:7]
    res <- merge(bib1, bib2, fields.to.check = c("author", "title"))
    expect_equal(length(res), 6L)
})

test_that("head and tail", {
    expect_length(head(bib), 6L)
    expect_equal(tail(bib, 3), bib[(length(bib)-2):length(bib)])
})

test_that("open", {
    skip_on_cran()
    open(as.BibEntry(citation("RCurl")))  # URL

    testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex"))
    testbib$file <- file.path(R.home("doc/manual"), "R-intro.pdf")
    open(testbib)  # PDF

    expect_message(open(testbib, open.field = "eprint"),
                   "Could not open the specified entry.")  # no error if cannot open

    open(bib["kastenholz"])  # DOI
    testbib <- BibEntry(bibtype = "Misc", key = "arxiv", eprinttype = "arxiv",
      eprintclass = "stat.ME", year = 2013, urldate = "2014-02-01", pubstate = "submitted",
      title = "Something On the {arXiv}", author = "Mathew W. McLean", eprint = "1403.2036")
    open(testbib, entry = 1)  # eprint
})

test_that("c", {
   expect_length(c(bib[1], bib[2]), 2L)
   expect_warning(c(bib[1], unlist(bib[2])))
})

test_that("levels", {
    expect_identical(levels(bib)[["angenendt"]], c("author", "title", "journaltitle", "date",
                                                   "volume", "pages", "langid", "indextitle",
                                                   "shorttitle", "annotation"))
    expect_identical(levels(bib[[1]])[[1]], c("author", "title", "subtitle", "pages",
                                              "crossref", "langid", "langidopts",
                                              "indextitle", "annotation"))
})

test_that("list extraction", {
    expect_length(bib[[c("westfahl:space", "angenendt")]], 2L)
    expect_length(bib[[6:10]], 5L)
})

