## test if textVersion attribute survives manipulations by BibEntry methods
## check other attributes as well

## methods(class="BibEntry")
##  [1] $.BibEntry             $<-.BibEntry           [.BibEntry
##  [4] [[.BibEntry            [[<-.BibEntry          [<-.BibEntry
##  [7] +.BibEntry             as.data.frame.BibEntry c.BibEntry
## [10] format.BibEntry        head.BibEntry          levels.BibEntry
## [13] merge.BibEntry         names.BibEntry         names<-.BibEntry
## [16] open.BibEntry          print.BibEntry         sort.BibEntry
## [19] tail.BibEntry          toBibtex.BibEntry      unlist.BibEntry
rm(list = ls(all=TRUE))
unloadNamespace("RefManageR")
library(RefManageR)

#
bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                   package = "RefManageR", lib.loc = .libPaths()), check = FALSE)[10:19]
attr(bib[[1]], "textVersion") <- "This is the text version of this citation"
BibOptions(style="textVersion")
bib

bib2 <- as.BibEntry(citation("mgcv"))
print(bib2, .opts = list(style = "textVersion"))

bib3 <- c(bib, bib2)
print(bib3, .opts = list(style = "textVersion"))

bib4 <- c(bibentry("misc", key = "test", title = "The title", author = "Bob Smith",
                   year = 2012), citation("mgcv"))
print(bib4[2], style = 'textVersion')
bib5 <- as.BibEntry(bib4)
# not working: only prints first reference
print(bib5[2], .opts = list(style = 'textVersion'))

tmp <- list(x = 1, y = 2)
attr(tmp[[1]], "att") <- "myatt"
