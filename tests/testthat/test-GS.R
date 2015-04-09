context("GScholar")

test_that("Can retrieve by dates, keep incomplete", {
    skip_on_cran()
    if (!RCurl::url.exists("http://scholar.google.com"))
        skip("Couldn't connect to GS")

    out <- ReadGS(scholar.id = "vqW0UqUAAAAJ", sort.by.date = TRUE,
                  check.entries = "warn", limit = 1)
    expect_is(out, "BibEntry")
    expect_equal(length(out), 1L)
})

test_that("check drop incomplete", {
    skip_on_cran()
    if (!RCurl::url.exists("http://scholar.google.com"))
        skip("Couldn't connect to GS")
    Sys.sleep(3)

    ## be cautious to not demand warnings as Scholar results sorted by dates may update
    ## using Leo Breiman should help prevent this from happening
    num.dropped <- 0
    lim <- 6
    frame.number <- sys.nframe()
    out <- withCallingHandlers(ReadGS(scholar.id = "mXSv_1UAAAAJ", limit = lim,
                                      sort.by.date = TRUE, check.entries = "error"),
      message = function(w){
        if (any(grepl("information for entry", w)))
           assign("num.dropped", num.dropped + 1L,
                  envir = sys.frame(frame.number))
        w
      })

    expect_is(out, "BibEntry")
    if (num.dropped > 0)
        expect_less_than(length(out), lim)
})

test_that("check read by cites", {
    skip_on_cran()
    if (!RCurl::url.exists("http://scholar.google.com"))
        skip("Couldn't connect to GS")

    Sys.sleep(5)
    expect_warning(out <- ReadGS(scholar.id = "CJOHNoQAAAAJ", check.entries = "warn",
                                 sort.by.date = FALSE, limit = 10), "Incomplete")
    expect_is(out, "BibEntry")
    expect_match(out[[1]]$title, "Measurement error")
})
