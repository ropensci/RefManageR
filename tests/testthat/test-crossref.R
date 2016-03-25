context("Crossref")

## GetBibEntryWithDOI
## GetDOIs
## ReadCrossRef

test_that("GetBibEntryWithDOIs retrieves DOIs", {
    skip_on_cran()
    if (!RCurl::url.exists("http://dx.doi.org/"))
        skip("Couldn't connect to dx.doi.org")

    dois <- c("10.1016/j.iheduc.2003.11.004", "10.3998/3336451.0004.203")
    out <- GetBibEntryWithDOI(dois)
    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
    dois.out <- setNames(unlist(out$doi), NULL)
    expect_equal(dois.out, dois)
})

test_that("GetBibEntryWithDOIs continues if some DOIs not found", {
    skip_on_cran()
    if (!RCurl::url.exists("http://dx.doi.org/"))
        skip("Couldn't connect to dx.doi.org")

    dois <- c("NotADOI", "10.3998/3336451.0004.203")
    out <- GetBibEntryWithDOI(dois)
    expect_is(out, "BibEntry")
    expect_equal(length(out), 1L)
    dois.out <- setNames(unlist(out$doi), NULL)
    expect_equal(dois.out, dois[2])
    expect_message(GetBibEntryWithDOI("crap"), "[Uu]nable to retrieve bibliographic")
})

test_that("GetDOIs retrieves DOIs", {
    skip_on_cran()
    if (!RCurl::url.exists("http://search.crossref.org"))
        skip("Couldn't connect to search.crossref.org")

    BibOptions(check.entries = FALSE, sorting = "none")
    bib <- ReadBib(system.file("Bib", "RJC.bib", package = "RefManageR"))[[3:4]]
    out <- GetDOIs(bib)

    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
    dois.out <- setNames(unlist(out$doi), NULL)
    expect_equal(dois.out, "10.1093/bioinformatics/btt608")
    expect_message(GetDOIs(out[[1]]), "All entries already have DOIs")
    ## expect_message(GetDOIs(out[[2]]), "No matches.")
})

old.opts <- BibOptions(check.entries = FALSE)

test_that("ReadCrossRef retrieves queries successfully", {
    skip_on_cran()
    if (!RCurl::url.exists("http://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    BibOptions(check.entries = FALSE, sorting = "none")    
    out <- ReadCrossRef(query = 'gelman bayesian', limit = 2, sort = "relevance",
        min.relevance = 80)

    expect_is(out, "BibEntry")
    expect_equal(length(out), 2L)
})

test_that("ReadCrossRef min.relevance and verbose args work", {
    skip_on_cran()
    if (!RCurl::url.exists("http://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    BibOptions(check.entries = FALSE, sorting = "none")    
    expect_message(ReadCrossRef(query = 'ruppert semiparametric regression',
                                       limit = 2, sort = "relevance",
                                       min.relevance = 100, verbose = TRUE),
                          regexp = "100\\n$")
})


test_that("ReadCrossRef works when given DOI", {
    skip_on_cran()
    if (!RCurl::url.exists("http://search.crossref.org/"))
        skip("Couldn't connect to search.crossref.org")

    out <- ReadCrossRef(query = "10.1007/978-1-4899-4477-1_13", limit = 2, sort = "relevance",
        min.relevance = 80)

    expect_is(out, "BibEntry")
    expect_equal(length(out), 1L)
})

BibOptions(old.opts)
