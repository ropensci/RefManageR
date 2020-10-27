library(testthat)
Sys.setenv(NOT_CRAN = "true")
## filter_patt <- "^(?!readPDF)[A-Za-z0-9]+$"
file.parts.to.exclude  <- c("readPDF", "crossref", "Read", "zotero")
exclude.patt <- paste0(file.parts.to.exclude, collapse = "|")
filter.patt <- if (!identical(Sys.getenv("NOT_CRAN"), "true"))
                       paste0("^(?!", exclude.patt, ")[A-Za-z0-9]+$",
                            collapse = "|")

test_check("RefManageR", perl = TRUE, filter = filter.patt)
