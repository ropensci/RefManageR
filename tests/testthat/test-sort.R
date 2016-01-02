context("Sorting")

##  nty - sort by name, then by title, then by year
##  nyt - sort by name, then by year, then title
##  nyvt - sort by name, year, volume, title
##  anyt - sort by alphabetic label, name, year, title
##  anyvt - sort by alphabetic label, name, year, volume, title
##  ynt - sort by year, name, title
##  ydnt - sort by year (descending), name, title
##  debug - sort by keys
##  none - no sorting is performed

## Searches for matches in a text file and returns the order in which
## the matches occur
## @param fname character; file name to read in
## @param strs character vector of regex patterns to look for in \code{fname}
## @section Warning:
##  Will break if any elements of strs are not found \emph{exactly once} in the
##  text file!!
## @return numeric vector with length \code{length(strs)}
grep.o <- function(fname, strs){
    txt <- readLines(fname)
    order(apply(sapply(strs, grepl, x = txt), 2, which))
}

expect_order <- function(ord, fname, strs)
    expect_equal(ord, grep.o(fname, strs))

file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
BibOptions(restore.defaults = TRUE)
bib <- suppressMessages(ReadBib(file.name)[[70:73]])
tfile <- tempfile(fileext = ".txt")

BIB <- as.BibEntry(citation())
BIB$author <- "John Smith and Jane Doe"
BIB <- c(BIB, BIB, BIB)
BIB$oragnization <- BIB$url <- NULL
BIB$address <- c("E1", "E2", "E3")
bib2 <- BIB

test_that("No sorting", {
    BibOptions(sorting = "none")
    capture.output(BIB, file = tfile)
    expect_order(1:3, tfile, c("E1", "E2", "E3"))
                 ##c("\\[KpV\\] I. Kant", "\\[KU\\] I. Kant", "Nachtheil", "Brandt"))
})

test_that("nyt sorting", {
    BibOptions(sorting = "nyt")
    bib2$author[2] <- "Adam Aardvark"
    bib2$year[3] <- "2008"
    capture.output(bib2, file = tfile)
    expect_order(c(2, 3, 1), tfile, c("E1", "E2", "E3"))
})

test_that("nyt with sortyear", {
    BibOptions(sorting = "nyt")
    bib2 <- BIB
    bib2$sortyear[2] <- paste(format(Sys.Date(), "%Y"), "2", sep = "-")
    capture.output(bib2, file = tfile)
    expect_order(c(1, 3, 2), tfile, c("E1", "E2", "E3"))
})

test_that("presort", {
    BibOptions(sorting = "nyt")
    bib2 <- BIB
    bib2$presort[2] <- "AA"
    capture.output(bib2, file = tfile)
    expect_order(c(2, 1, 3), tfile, c("E1", "E2", "E3"))
})

test_that("sortkey, so nothing else used", {
    BibOptions(sorting = "nyt")
    bib2 <- BIB
    bib2$sortkey <- 3:1
    bib2$year <- c(2001, 2002, 2003)
    capture.output(bib2, file = tfile)
    expect_order(3:1, tfile, c("E1", "E2", "E3"))
})

test_that("ynt", {
    BibOptions(sorting = "ynt")
    bib2 <- BIB
    bib2$year <- c(2001, 2002, 2003)
    capture.output(bib2, file = tfile)
    expect_order(1:3, tfile, c("E1", "E2", "E3"))
    bib2$year <- c(2001, 2002, 2002)
    bib2$title <- c("C", "B", "A")
    capture.output(bib2, file = tfile)
    expect_order(c(1, 3, 2), tfile, c("E1", "E2", "E3"))
})

test_that("ydnt", {
    BibOptions(sorting = "ydnt")
    bib2 <- BIB
    bib2$year <- c(2001, 2002, 2003)
    capture.output(bib2, file = tfile)
    expect_order(3:1, tfile, c("E1", "E2", "E3"))
})

test_that("nyvt and sorttitle", {
    BibOptions(sorting = "nyvt")
    bib2 <- BIB
    bib2$sorttitle[3] <- "AA"
    bib2$volume <- c("3", "2", "2")
    capture.output(bib2, file = tfile)
    expect_order(c(3, 2, 1), tfile, c("E1", "E2", "E3"))
})

test_that("nyvt, missing volumes first", {
    BibOptions(sorting = "nyvt")
    bib2 <- BIB
    bib2$volume[c(1, 3)] <- c("3", "2")
    capture.output(bib2, file = tfile)
    expect_order(c(2, 3, 1), tfile, c("E1", "E2", "E3"))
})


test_that("debug", {
    BibOptions(sorting = "debug")
    bib2 <- BIB
    bib2$key <- c("aa", "cc", "bb")
    capture.output(bib2, file = tfile)
    expect_order(c(1, 3, 2), tfile, c("E1", "E2", "E3"))
})

test_that("nty", {
    BibOptions(sorting = "nty")
    bib2 <- BIB
    bib2$editor <- c("Jane Austin", "Adam Aardvark", "Jane Austin")
    bib2$author <- NULL
    bib2$title <- c("bb", "cc", "aa")
    capture.output(bib2, file = tfile)
    expect_order(c(2, 3, 1), tfile, c("E1", "E2", "E3"))
})

test_that("anyt", {
    BibOptions(sorting = "anyt")
    bib2 <- BIB
    bib2$label <- c("aa", "cc", "bb")
    capture.output(bib2, file = tfile)
    expect_order(c(1, 3, 2), tfile, c("E1", "E2", "E3"))
})

test_that("shorthand and sortname", {
    BibOptions(sorting = "anyt")
    bib2 <- BIB
    bib2$label <- c("dd", "cc", "bb")
    bib2$shorthand[2] <- "aa"
    capture.output(bib2, file = tfile)
    expect_order(c(2, 3, 1), tfile, c("E1", "E2", "E3"))
})


test_that("anyvt (and not just a)", {
    BibOptions(sorting = "anyvt")
    bib2 <- BIB
    bib2$volume <- c("3", "2", "2")
    bib2$label <- c("bb", "aa", "bb")
    capture.output(bib2, file = tfile)
    expect_order(c(2, 3, 1), tfile, c("E1", "E2", "E3"))
})

unlink(tfile)
