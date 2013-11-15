require(scholar)
require(RCurl)
require(XML)
require(stringr)

# str_replace_all(temp, fixed('â€'), '-')

ReadGS <- function(scholar.id, start = 0, limit = 100, sort.by.date = FALSE,
                     encoding = 'UTF-8', bib.violation = .BibOptions$bib.violation){
  limit <- min(limit, 100)
  ps <- ifelse(limit <= 20, 20, 100)
  oldvio <- .BibOptions$bib.violation
  .BibOptions$bib.violation <- bib.violation
  .params <- list(hl = 'en', user = scholar.id, oe = encoding, pagesize = ps,
                  view_op = 'list_works', cstart = start)
  if(sort.by.date)
    .params$sortby = 'pubdate'
  
  uri <- 'http://scholar.google.com/citations'
  
  doc <- getForm(uri, .params = .params, .encoding = encoding)
  cites <- xpathApply(htmlParse(doc, encoding = encoding), "//tr[@class=\"cit-table item\"]")
  if(!length(cites)){
    message('No results.')
    return()
  }
  cites <- cites[1:min(limit, length(cites))]
  tmp <- lapply(cites, ParseGSCites)
  out <- lapply(tmp[!is.na(tmp)], MakeBibEntry, GS = TRUE)
  out <- MakeCitationList(out)
  
  .BibOptions$bib.violation <- oldvio
  
  return(out)
}
