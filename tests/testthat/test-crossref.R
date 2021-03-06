context("Crossref")

## GetBibEntryWithDOI
## GetDOIs
## ReadCrossRef

test_that("GetBibEntryWithDOIs retrieves DOIs", {
    skip_on_cran()
    if (httr::http_error("https://doi.org/"))
        skip("Couldn't connect to doi.org")

    dois <- c("10.1016/j.iheduc.2003.11.004", "10.3998/3336451.0004.203")
    out <- GetBibEntryWithDOI(dois)
    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
    dois.out <- setNames(unlist(out$doi), NULL)
    expect_equal(dois.out, dois)
})

test_that("GetBibEntryWithDOIs continues if some DOIs not found", {
    skip_on_cran()
    if (httr::http_error("https://doi.org/"))
        skip("Couldn't connect to doi.org")

    dois <- c("NotADOI", "10.3998/3336451.0004.203")
    Sys.sleep(3)
    try_again(3, out <- GetBibEntryWithDOI(dois))
    expect_is(out, "BibEntry")
    expect_equal(length(out), 1L)
    dois.out <- setNames(unlist(out$doi), NULL)
    expect_equal(dois.out, dois[2])
    expect_message(GetBibEntryWithDOI("crap"),
                   "[Uu]nable to retrieve bibliographic")
})

## test_that("GetDOIs retrieves DOIs", {
##     skip_on_cran()
##     if (httr::http_error("https://search.crossref.org"))
##         skip("Couldn't connect to search.crossref.org")

##     BibOptions(check.entries = FALSE, sorting = "none")
##     bib <- ReadBib(system.file("Bib", "RJC.bib",
##                                package = "RefManageR"))[[3:4]]
##     out <- GetDOIs(bib)

##     expect_is(out, "BibEntry")
##     expect_equal(length(out), 2L)
##     dois.out <- setNames(unlist(out$doi), NULL)
##     expect_equal(dois.out[1], "10.1093/bioinformatics/btt608")
##     expect_message(GetDOIs(out[[1]]), "All entries already have DOIs")
##     ## expect_message(GetDOIs(out[[2]]), "No matches.")
## })

old.opts <- BibOptions(check.entries = FALSE)

test_that("ReadCrossRef *old* API warns and uses new API", {
    skip_on_cran()
    if (httr::http_error("https://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    BibOptions(check.entries = FALSE, sorting = "none")
    Sys.sleep(3)
    expect_warning(try_again(3,
                             out <- ReadCrossRef(query = 'gelman bayesian',
                                                 limit = 2, sort = "relevance",
                                                 min.relevance = 20,
                                                 use.old.api = TRUE)),
                   "old CrossRef API is no longer support")

    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
})

test_that("ReadCrossRef *new* API retrieves queries successfully", {
    skip_on_cran()
    if (httr::http_error("https://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    BibOptions(check.entries = FALSE, sorting = "none")
    Sys.sleep(2)
    try_again(3, out <- ReadCrossRef("regression", filter = list(prefix="10.1198"),
                        limit = 2, offset = 1))

    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
})


## test_that("ReadCrossRef *old* API min.relevance and verbose args work", {
##     skip_on_cran()
##     if (httr::http_error("https://search.crossref.org/"))
##         skip("Couldn't connect to search.crossref.org")

##     BibOptions(check.entries = FALSE, sorting = "none")
##     expect_message(ReadCrossRef(query = 'ruppert semiparametric regression',
##                                        limit = 2, sort = "relevance",
##                                 min.relevance = 100, verbose = TRUE,
##                                 use.old.api = TRUE),
##                           regexp = "Ruppert")
## })


test_that("ReadCrossRef works when given DOI", {
    skip_on_cran()
    if (httr::http_error("https://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    Sys.sleep(3)
    try_again(10, out <- ReadCrossRef(query = "10.1007/978-1-4899-4477-1_13",
                                     limit = 1, sort = "relevance",
                                     min.relevance = 0))

    expect_is(out, "BibEntry")
    expect_equal(length(out), 1L)
})

BibOptions(old.opts)
