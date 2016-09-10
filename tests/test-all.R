library(testthat)
## Sys.setenv(NOT_CRAN = "true")
test_check("RefManageR", perl = TRUE, filter = "^(?!readPDF)[A-Za-z0-9]+$")
