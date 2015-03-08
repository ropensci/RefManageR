context("Searching")

## unloadNamespace("RefManageR")
## library(RefManageR)

file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- suppressMessages(ReadBib(file.name, check = FALSE))

test_that("author search via family names only", {
    expect_equal(length(bib[author = "aristotle"]), 4)
})

## Aristotle references before 1925
test_that("author and year interval search, no start year", {
    expect_equal(length(bib[author="aristotle", date = "/1925"]), 2L)
})

## ## Aristotle references before 1925 *OR* references with editor Westfahl
## test_that("'OR' search using list", {
##     rm(list=ls())
##     unloadNamespace("RefManageR")
##     library(RefManageR)
##     file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
##     bE <- suppressMessages(ReadBib(file.name))
##     if (!exists("bE") || length(bE) == 0L)
##         skip("Couldn't read biblatexExamples.bib")

##     len <- length(bE[list(author="aristotle", date = "/1925"),
##                             list(editor = "westfahl")])
##     ## print(len)
##     ## print(bib[list(author="aristotle", date = "/1925"),
##     ##                         list(editor = "westfahl")])
##     stopifnot(len==4L)
## })

## Change some searching and printing options and search for author
test_that("full name search", {
    old.opts <- BibOptions(bib.style = "authoryear", match.author = "exact",
                           max.names = 99L, first.inits = FALSE)
    expect_equal(length(bib[author="Mart\u00edn, Jacinto and S\u00e1nchez, Alberto"]), 1L)
    BibOptions(old.opts)  ## reset options
})

file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
bib2 <- suppressMessages(ReadBib(file.name))


## ## index by key
test_that("search by key", {
    expect_equal(length(bib2[c("chen2013using", "carroll1978distributions")]), 2L)
})

## ## Papers with someone with family name Wang
test_that("search more authors", {
    expect_equal(length(SearchBib(bib2, author='Wang',
                              .opts = list(match.author = "family"))), 37L)
})

## ## Papers with Wang, N.
test_that("author with initials", {
    expect_equal(length(SearchBib(bib2, author='Wang, N.',
                              .opts = list(match.author = "family.with.initials"))), 19L)
})

## ## tech reports with Ruppert
test_that("search with author and bibtype", {
    expect_equal(length(bib2[author='ruppert',bibtype="report"]), 19L)
})

## ##Carroll and Ruppert tech reports at UNC
test_that("search with bibtype and institution", {
    expect_equal(length(bib2[author='ruppert',bibtype="report",institution="north carolina"]), 10L)
})

## ## Carroll and Ruppert papers since leaving UNC
test_that("exact date search", {
    expect_equal(length(SearchBib(bib2, author='ruppert', date="1987-07/",
                     .opts = list(match.date = "exact"))), 53L)
})

## ## Carroll and Ruppert papers NOT in the 1990's
test_that("search excluding dates", {
    expect_equal(length(SearchBib(bib2, author='ruppert', date = "!1990/1999")), 59L)
})

test_that("searching year is same as searching date", {
    expect_identical(SearchBib(bib2, author='ruppert', date = "!1990/1999"),
              SearchBib(bib2, author='ruppert', year = "!1990/1999"))
})
## Carroll + Ruppert + Simpson
test_that("three author search", {
    expect_equal(length(bib2[author="Carroll, R. J. and Simpson, D. G. and Ruppert, D."]), 5L)
})

## ## Carroll + Ruppert OR Carroll + Simpson
test_that("search with author vector (OR)", {
    expect_equal(length(bib2[author=c("Carroll, R. J. and Ruppert, D.",
                                     "Carroll, R. J. and Simpson, D. G.")]), 94L)
})

## Carroll + Ruppert tech reports at UNC OR Carroll and Ruppert JASA papers
## test_that("OR search, several fields", {
##     rm(list=ls())
##     unloadNamespace("RefManageR")
##     library(RefManageR)
##     file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
##     bib2 <- suppressMessages(ReadBib(file.name))
##     if (!exists("bib2") || length(bib2) == 0L)
##         skip("Couldn't read RJC bib")
##     len <- length(bib2[list(author='ruppert',bibtype="report",institution="north carolina"),
##                        list(author="ruppert",journal="journal of the american statistical association")])

##     stopifnot(len==22L)
## })
