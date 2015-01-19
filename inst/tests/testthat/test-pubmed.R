context("PubMed")

test_that("GetPubMedByID can process books (#2)", {
  test <- GetPubMedByID(c("24977996", "24921111"))
  if (length(test))
    expect_true(length(test) == 2L)
})

test_that("GetPubMedByID will use collective name if individual authors missing (#2)", {
  test <- GetPubMedByID(c(11678951, 15373863))
  if (length(test)){
    authors <- unlist(test$author)
    expect_true(length(authors) == 2L)
  }
})
