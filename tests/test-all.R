library(testthat)
## Sys.setenv(NOT_CRAN = "true")
test_check("RefManageR", filter = "^(?!readPDF)[A-Za-z0-9]+$", perl = TRUE)
