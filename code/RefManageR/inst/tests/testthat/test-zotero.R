rm(list = ls(all=TRUE))
unloadNamespace("RefManageR")
library(RefManageR)
#BibOptions(restore.defaults = TRUE)
expect_length <- function(expr, len){
  expect_equal(length(expr), len)
}

expect_equal(length(ReadZotero(user='1648676', .params=list(q='bayesian', key='7lhgvcwVq60CDi7E68FyE3br', 
   limit=2))), 2L)

 ## Search specific collection
 ## collection key can be found by reading uri when collection is selected in Zotero
expect_equal(length(ReadZotero(user='1648676', .params=list(q='yu', key='7lhgvcwVq60CDi7E68FyE3br', 
   collection='3STEQRNU'))), 3L)

 ## Search by tag
 ## Notice issue with how Zotero uses Techreport entry for arXiv manuscripts
BibOptions(check.entries = "error") 
expect_length(suppressMessages(ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', 
   tag='Statistics - Machine Learning'))), 0L)
BibOptions(check.entries = FALSE) 
expect_length(suppressMessages(ReadZotero(user='1648676', 
                  .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))), 10L)
