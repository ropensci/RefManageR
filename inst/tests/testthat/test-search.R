context("Searching")

rm(list = ls(all=TRUE))
unloadNamespace("RefManageR")
library(RefManageR)
file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- suppressMessages(ReadBib(file.name))

## author search, default is to use family names only for matching
expect_equal(length(bib[author = "aristotle"]), 4L)

## Aristotle references before 1925
expect_equal(length(bib[author="aristotle", date = "/1925"]), 2L)

## Aristotle references before 1925 *OR* references with editor Westfahl
expect_equal(length(bib[list(author="aristotle", date = "/1925"),
                        list(editor = "westfahl")]), 4L)

## Change some searching and printing options and search for author
old.opts <- BibOptions(bib.style = "authoryear", match.author = "exact",
                       max.names = 99L, first.inits = FALSE)
expect_equal(length(bib[author="Mart\u00edn, Jacinto and S\u00e1nchez, Alberto"]), 1L)
BibOptions(old.opts)  ## reset options

## index by key
expect_equal(length(bib[c("chen2013using", "carroll1978distributions")]), 2L)

## Papers with someone with family name Wang
expect_equal(length(SearchBib(bib, author='Wang',
                              .opts = list(match.author = "family"))), 37L)

## Papers with Wang, N.
expect_equal(length(SearchBib(bib, author='Wang, N.',
                              .opts = list(match.author = "family.with.initials"))), 19L)

## tech reports with Ruppert
expect_equal(length(bib[author='ruppert',bibtype="report"]), 19L)

##Carroll and Ruppert tech reports at UNC
expect_equal(length(bib[author='ruppert',bibtype="report",institution="north carolina"]), 10L)

## Carroll and Ruppert papers since leaving UNC
expect_equal(length(SearchBib(bib, author='ruppert', date="1987-07/",
                 .opts = list(match.date = "exact"))), 53L)

## Carroll and Ruppert papers NOT in the 1990's
expect_equal(length(SearchBib(bib, author='ruppert', date = "!1990/1999")), 59L)
identical(SearchBib(bib, author='ruppert', date = "!1990/1999"),
          SearchBib(bib, author='ruppert', year = "!1990/1999"))

## Carroll + Ruppert + Simpson
expect_equal(length(bib[author="Carroll, R. J. and Simpson, D. G. and Ruppert, D."]), 5L)

## Carroll + Ruppert OR Carroll + Simpson
expect_equal(length(bib[author=c("Carroll, R. J. and Ruppert, D.",
                                 "Carroll, R. J. and Simpson, D. G.")]), 94L)

## Carroll + Ruppert tech reports at UNC OR Carroll and Ruppert JASA papers
expect_equal(length(bib[list(author='ruppert',bibtype="report",institution="north carolina"),
           list(author="ruppert",journal="journal of the american statistical association")]),
           22L)
