context("PubMed")

## GetPubMedByID
## GetPubMedRelated
## ReadGS
## ReadPubMed


test_that("GetPubMedByID can process books (#2)", {
  skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")

  test <- GetPubMedByID(c("24977996", "24921111"))
  if (length(test))
    expect_true(length(test) == 2L)
})

test_that("GetPubMedByID uses collective name if authors missing (#2)", {
  skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")
  Sys.sleep(2)

  try_again(3, test <- GetPubMedByID(c(11678951, 15373863)))
  if (length(test)){
    authors <- unlist(test$author)
    expect_true(length(authors) == 2L)
  }
})

test_that("GetPubMedByID warns if authors missing (#3)", {
  skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")
  Sys.sleep(2)

  BibOptions(check.entries = FALSE)
  expect_warning(try_again(3, GetPubMedByID("7936917")))
})

test_that("LookupPubMedID successfully retrieves and add ID's'", {
    skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")
    
    file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
    bib <- ReadBib(file.name)
    Sys.sleep(2)
    try_again(3, out <- LookupPubMedID(bib[[101:102]]))
    expect_equal(length(out), 2L)
    ids <- setNames(unlist(out$eprint), NULL)
    expect_equal(ids, c("19381352", "19444335"))
    Sys.sleep(2)
    expect_message(try_again(3, LookupPubMedID(bib, 453)), "No PubMed ID's found")
})

test_that("GetPubMedByID reading of years/months (#52)", {
  skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")
  Sys.sleep(2)
  try_again(3, bib <- GetPubMedByID("23891459"))
  expect_equal(bib$year, "2013")
  expect_equal(bib$month, "07")
})

test_that("GetPubMedByID: Multiple books parsed correctly #86",
{
    skip_if_offline("https://eutils.ncbi.nlm.nih.gov/")
    ids <- c("33780208", "33764725")
    names(ids) <- c("geary2021variation", "brennan2021potential")
    try_again(3, bib <- GetPubMedByID(ids))
    expect_equal(unlist(bib$eprint), ids)
})
