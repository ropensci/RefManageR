library(RefManageR)
bib <- ReadPubMed('12621314[pmid] or 16639167[pmid] or 14552507[pmid] or 11950548[pmid]')

GetDOIs(bib)$doi

addDOI <- function(bib, n.retry = 5){
  library(XML)
  miss.doi  <- which(sapply(bib, function(x) is.null(x$doi)))
  pb <- txtProgressBar(min = 0, max = length(miss.doi), style = 3)
  for(i in miss.doi) {
    setTxtProgressBar(pb, which(miss.doi == i))
    search.string <- paste0(
      ifelse(!is.null(bib[i]$issn), paste0('&issn=', bib[i]$issn), ''), 
      paste0('&aulast=', paste(bib[i]$author[[1]]$family, collapse = ' ')),
      ifelse(!is.null(bib[i]$pages), paste0('&spage=', sub('-.*', '', bib[i]$pages)),
             paste0('&atitle=', substr(bib[i]$title, 1, 70))),
      ifelse(!is.null(bib[i]$volume), paste0('&volume=', bib[i]$volume), ''),
      paste0('&date=', ifelse(is.null(bib[i]$year), bib[i]$date, bib[i]$year)))
    if(is.null(bib[i]$issn) | is.null(bib[i]$volume)){
      search.string <- paste(c(search.string, '&title=', bib[i]$journal), collapse = '')
    }
    search.string <- URLencode(gsub('\\{|\\}', '', search.string))
    search.string <- paste(c('http://www.crossref.org/openurl?multihit=true',
                             '&pid=erich.studerus@gmail.com', search.string), collapse = '')
    capture.output(doc <- try(xmlTreeParse(search.string), silent = T))
    n <- 1
    if(inherits(doc, 'try-error')){
      while(n < n.retry) {
        Sys.sleep(0.5)
        capture.output(doc <- try(xmlTreeParse(search.string), silent = T))
        if(inherits(doc, 'try-error')){
          n <- n + 1
        } else {
          n <- n.retry
        }
      }
    }
    root <- xmlRoot(doc)
    doi <- xpathSApply(root, "//x:doi", namespaces = "x")$children$text$value 
    if(!is.null(doi)) {
      bib[[i]]$doi <- doi 
    } 
  }
  bib
}

addDOI(bib)$doi
