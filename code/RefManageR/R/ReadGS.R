require(scholar)
require(RCurl)
require(XML)
require(stringr)

# str_replace_all(temp, fixed('â€'), '-')
#' Import book and article references from a public Google Scholar profile by ID.
#' 
#' This function will create a BibEntry object for up to 100 references from a provided Google Scholar ID, 
#' if the profile is public.  The number of citations for each entry will also be imported.
#' 
#' @param scholar.id - character; the Google Scholar ID from which citations will be imported.  The ID can by found by 
#' visiting an author's Google Scholar profile and noting the value in the uri for the \dQuote{user} parameter.
#' @param start - numeric; index of first citation to include.
#' @param limit - numeric; maximum number of results to return.  Cannot exceed 100.
#' @param sort.by.date - boolean; if true, newest citations are imported first; otherwise, most cited works are imported first.
#' @param .Encoding - character; text encoding to use for importing the results and creating the bib entries.
#' @param bib.violation - .BibOptions$
ReadGS <- function(scholar.id, start = 0, limit = 100, sort.by.date = FALSE,
                     .Encoding = 'UTF-8', bib.violation = .BibOptions$bib.violation){
  limit <- min(limit, 100)
  ps <- ifelse(limit <= 20, 20, 100)
  oldvio <- .BibOptions$bib.violation
  .BibOptions$bib.violation <- bib.violation
  .params <- list(hl = 'en', user = scholar.id, oe = .Encoding, pagesize = ps,
                  view_op = 'list_works', cstart = start)
  if (sort.by.date)
    .params$sortby = 'pubdate'
  
  uri <- 'http://scholar.google.com/citations'
  
  doc <- getForm(uri, .params = .params, .encoding = .Encoding)
  cites <- xpathApply(htmlParse(doc, encoding = .Encoding), "//tr[@class=\"cit-table item\"]")
  if(!length(cites)){
    message('No results.')
    return()
  }
  cites <- cites[1:min(limit, length(cites))]
  tmp <- lapply(cites, ParseGSCites)
  out <- lapply(tmp[!is.na(tmp)], MakeBibEntry, to.person = FALSE)
  out <- MakeCitationList(out)
  
  .BibOptions$bib.violation <- oldvio
  
  return(out)
}
