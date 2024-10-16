context("PubMed")

## GetPubMedByID
## GetPubMedRelated
## ReadGS
## ReadPubMed

TestPubMed <- function(name, ids, response, expect_code, warn = FALSE)
{
  error <- ""
  Sys.sleep(3)
  if (warn)
  {
    BibOptions(check.entries = FALSE)
    expect_output <- expect_warning
  }else{
    BibOptions(check.entries = TRUE)
    expect_output <- expect_silent
  }
  warnings <- capture_warning(bib <- try(GetPubMedByID(ids), TRUE))
  test_that(name,
  {
    if (inherits(bib, "try-error"))
    {
      error <- attr(bib, "condition")$message
      bib <- with_mocked_bindings(POST = function(...)
        response,
        expect_output(GetPubMedByID(ids)))
    }else if (warn)
      expect_length(warnings, 2L)

    eval(expect_code)
  })
  return(error)
}

responses <- readRDS("pubmed_responses.rds")
test.names <- names(responses)
ids <- setNames(list(c("24977996", "24921111"),
                     c(11678951, 15373863),
                     "7936917",
                     "23891459",
                     setNames(c("33780208", "33764725"),
                              c("geary2021variation", "brennan2021potential"))),
                test.names)
warnings <- logical(length(responses))
warnings[grepl("warn", names(responses), fixed = TRUE)] <- TRUE

expect_code <- list(quote(expect_length(bib, 2L)),
                   quote({
                     authors <- unlist(bib$author)
                     expect_true(length(authors) == 2L)
                   }),
                   quote(expect_length(bib, 1L)),
                   quote({
                     expect_equal(bib$year, "2013")
                     expect_equal(bib$month, "07")
                   }),
                   quote(expect_equal(unlist(bib$eprint), ids)))
errors <- Map(TestPubMed, test.names, ids, responses, expect_code, warnings)
all.errored <- all(vapply(errors, nchar, 1L) > 0)
expect_false(all.errored, label = paste(c("All GetPubMedbyID API requests failed",
                                          errors), collapse = "\n"))
 
test_that("LookupPubMedID successfully retrieves and add ID's'", {
    skip_if_offline("eutils.ncbi.nlm.nih.gov")

    file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
    bib <- ReadBib(file.name)
    Sys.sleep(3)
    try_again(3, out <- LookupPubMedID(bib[[101:102]]))
    expect_equal(length(out), 2L)
    ids <- setNames(unlist(out$eprint), NULL)
    expect_equal(ids, c("19381352", "19444335"))
    Sys.sleep(3)
    expect_message(try_again(3, LookupPubMedID(bib, 453)), "No PubMed ID's found")
})
