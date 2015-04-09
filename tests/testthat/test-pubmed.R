context("PubMed")

## GetPubMedByID
## GetPubMedRelated
## ReadGS
## ReadPubMed


test_that("GetPubMedByID can process books (#2)", {
  skip_on_cran()
  test <- GetPubMedByID(c("24977996", "24921111"))
  if (length(test))
    expect_true(length(test) == 2L)
})

test_that("GetPubMedByID will use collective name if individual authors missing (#2)", {
  skip_on_cran()
  test <- GetPubMedByID(c(11678951, 15373863))
  if (length(test)){
    authors <- unlist(test$author)
    expect_true(length(authors) == 2L)
  }
})

test_that("GetPubMedByID warns if authors missing (#3)", {
    skip_on_cran()
    gives_warning(GetPubMedByID("7936917"))
})

test_that("LookupPubMedID successfully retrieves and add ID's'", {
    skip_on_cran()
    file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
    bib <- ReadBib(file.name)
    if (!RCurl::url.exists("http://eutils.ncbi.nlm.nih.gov/"))
        skip("Couldn't connect to Entrez")
    out <- LookupPubMedID(bib[[101:102]])
    expect_equal(length(out), 2L)
    ids <- setNames(unlist(out$eprint), NULL)
    expect_equal(ids, c("19381352", "19444335"))
    expect_message(LookupPubMedID(bib, 453), "No PubMed ID's found")
})

