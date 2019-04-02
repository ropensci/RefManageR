context("Bibentry methods")

expect_length <- function(obj, len){
    expect_equal(length(obj), len)
}

bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                           package = "RefManageR"), check = FALSE)


test_that("yaml printing with authoryear", {
    BibOptions(check.entries = FALSE)
    idx <- if (Sys.getenv("NOT_CRAN") == "")
               47:92
           else
               48:50
    print(bib[idx], .opts = list(bib.style = "authoryear",
                                   sorting = "anyvt", style = "yaml"))
})

test_that("R style printing with authoryear", {
    idx <- if (Sys.getenv("NOT_CRAN") == "true")
               1:46
           else
               8:10
    print(bib[idx], .opts = list(bib.style = "authoryear",
                                  sorting = "anyt", style = "R"))
})


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

test_that("addition operator ignore.case",
{
    bib1 <- BibEntry(key = "k1", title = "THE TITLE", author = "Smith, John",
                     bibtype = "Misc", year = 2018)
    bib2 <- BibEntry(key = "k2", title = "The Title", author = "Smith, John",
                     bibtype = "Misc", year = 2018)
    BibOptions(ignore.case = TRUE, merge.fields.to.check = c("year", "title"))
    expect_message(bib1 + bib2, "Only duplicates")
    expect_length(merge(bib1, bib2, ignore.case = FALSE,
                        fields.to.check = "title"), 2)
})

test_that("head and tail", {
    expect_length(head(bib), 6L)
    expect_equal(tail(bib, 3), bib[(length(bib)-2):length(bib)])
})

test_that("open", {
    skip_on_cran()
    skip_on_travis()
    open(as.BibEntry(citation("httr")))  # URL

    testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex"))
    testbib$file <- file.path(R.home("doc/manual"), "R-intro.pdf")
    open(testbib)  # PDF

    ## no error if cannot open
    expect_message(open(testbib, open.field = "eprint"),
                   "Could not open the specified entry.")

    open(bib["kastenholz"])  # DOI
    testbib <- BibEntry(bibtype = "Misc", key = "arxiv", eprinttype = "arxiv",
                        eprintclass = "stat.ME", year = 2013,
                        urldate = "2014-02-01", pubstate = "submitted",
                        title = "Something On the {arXiv}",
                        author = "Mathew W. McLean", eprint = "1403.2036")
    open(testbib, entry = 1)  # eprint
})

test_that("Warning for unknown macro includes key",
{
    bib <- BibEntry(key = "testkey", bibtype = "misc", author = "Smith, John",
                    title = "Title with \\badmacro", year = 2018)
    expect_warning(print(bib), "^testkey: unknown macro")
})

test_that("c", {
   expect_length(c(bib[1], bib[2]), 2L)
   expect_error(c(bib[1], unlist(bib[2])))
   expect_is(c(bib[1], NULL), "BibEntry")
})

test_that("levels", {
    expect_identical(levels(bib)[["angenendt"]],
                     c("author", "title", "journaltitle", "date", "volume",
                       "pages", "langid", "indextitle", "shorttitle",
                       "annotation"))
    expect_identical(levels(bib[[1]])[[1]],
                     c("author", "title", "subtitle", "pages", "crossref",
                       "langid", "langidopts", "indextitle", "annotation"))
})

test_that("list extraction", {
    expect_length(bib[[c("westfahl:space", "angenendt")]], 2L)
    expect_length(bib[[6:10]], 5L)
})

test_that("#62 no Unicode char. convers. when addPeriod", {
    bib.text <- "@article{HENRIQUES201715670,
          title = \"{{A Discontinuous Galerkin Method for optimal and sub-optimal control applied to an oscillating water column wave energy converter}}\",
          journal = \"IFAC-PapersOnLine\",
          volume = \"50\",
          number = \"1\",
          pages = \"15670 - 15677\",
          year = \"2017\",
          doi = \"10.1016/j.ifacol.2017.08.2400\",
          author = \"J. C. C. Henriques and J. M. Lemos and L. E\\c{c}a and J. N. H. Val\\'erio and L. M. C. Gato and A. F. O. Falcao\",
    }"
    tfile <- tempfile()
    writeLines(bib.text, tfile)
    bib <- ReadBib(tfile, check = FALSE)
    BibOptions(bib.style = "numeric", first.inits=TRUE, max.names = 5)
    sink(tfile)
    bib
    sink()
    out <- readLines(tfile)
    expect_true(grepl("Eça", out[1], useBytes = TRUE))
    expect_true(grepl("Valério", out[1], useBytes = TRUE))
})
