context("Zotero")

## rm(list = ls(all=TRUE))
## unloadNamespace("RefManageR")
## library(RefManageR)
#BibOptions(restore.defaults = TRUE)
expect_length <- function(expr, len){
  expect_equal(length(expr), len)
}

test_that("basic Zotero search", {
   ## if (!RCurl::url.exists("http://api.zotero.org/users"))
    ##     skip("Couldn't connect to Zotero")
   res <- ReadZotero(user='1648676', .params=list(q='bayesian', key='7lhgvcwVq60CDi7E68FyE3br',
                                         limit=2))
   if (!length(res))
       skip("Couldn't connect to Zotero")
   expect_length(res, 2L)
})

test_that("Search specific collection", {
   ## if (!RCurl::url.exists("http://api.zotero.org/users"))
    ##   skip("Couldn't connect to Zotero")
   Sys.sleep(2)
   res <- ReadZotero(user='1648676', .params=list(q='yu', key='7lhgvcwVq60CDi7E68FyE3br',
                                         collection='3STEQRNU'))
   if (!length(res))
       skip("Couldn't connect to Zotero")
   expect_length(res, 3L)
})

test_that("Search by tag", {
   ## Notice issue with how Zotero uses Techreport entry for arXiv manuscripts
   ## if (!RCurl::url.exists("http://api.zotero.org/users"))
   ##     skip("Couldn't connect to Zotero")
   Sys.sleep(2)
   BibOptions(check.entries = "error")
   res <- suppressMessages(ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br',
                                                          tag='Statistics - Machine Learning')))
   expect_length(res, 0L)
   BibOptions(check.entries = FALSE)
   res <- suppressMessages(ReadZotero(user='1648676',
                                      .params=list(key='7lhgvcwVq60CDi7E68FyE3br',
                                          tag='Statistics - Machine Learning')))
   if (!length(res))
       skip("Couldn't connect to Zotero")
   expect_length(res, 10L)
})
