## These tests work here on their own but not when included in test-search.R
## They both are 'OR' searches: calls to `[.BibEntry` with args. i and j present and
## lead to `[.BibEntry` being called recursively (unlike any calls currently in test-search.R

context("'OR' searches with lists")

test_that("OR search, several fields", {
    ## rm(list=ls())
    ## unloadNamespace("RefManageR")
    ## library(RefManageR)
    file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
    bib2 <- suppressMessages(ReadBib(file.name))

    len <- length(bib2[list(author='ruppert',bibtype="report",institution="north carolina"),
                       list(author="ruppert",journal="journal of the american statistical association")])

    expect_equal(len, 22L)
})

## Aristotle references before 1925 *OR* references with editor Westfahl
test_that("'OR' search using list", {
    ## rm(list=ls())
    ## unloadNamespace("RefManageR")
    ## library(RefManageR)

    file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
    bib <- suppressMessages(ReadBib(file.name))

    len <- length(bib[list(author="aristotle", date = "/1925"),
                            list(editor = "westfahl")])
    ## print(len)
    ## print(bib[list(author="aristotle", date = "/1925"),
    ##                         list(editor = "westfahl")])
    expect_equal(len, 4L)
})
