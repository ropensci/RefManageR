###########################################################################
# XML + google scholar
# https://stat.ethz.ch/pipermail/r-help/2009-September/212008.html
###########################################################################
library(RCurl)
library(XML)
# http://scholar.google.com/scholar?as_q=measurement+error&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=rj+carroll&as_publication=journal+of+the+american+statistical+association&as_ylo=&as_yhi=&btnG=&hl=en&as_sdt=0%2C44
# http://scholar.google.com/scholar?as_q=measurement+error&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=rj+carroll&as_publication=journal+of+the+american+statistical+association&as_ylo=1996&as_yhi=&btnG=&hl=en&as_sdt=0%2C44
# http://scholar.google.com/scholar?as_q=measurement+error&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=rj+carroll&as_publication=journal+of+the+american+statistical+association&as_ylo=1996&as_yhi=2005&btnG=&hl=en&as_sdt=0%2C44
GetBibtexGS <- function(title, authors='', journal='', low.year='', high.year='', exact=FALSE, curl = getCurlHandle()){
  # paste(strsplit('rj carroll', ' ')[[1]], collapse='+')
  argh <- list(uri="http://scholar.google.com/scholar", as_q='', as_authors=authors, as_publication=journal, as_ylo=low.year, 
               as_yhi=high.year, hl='en', btnG='Search', curl=curl, .opts=list(verbose=FALSE))
  search.t <- ifelse(exact, 'as_epq', 'as_q')
  argh[[search.t]] <- title
  #   z  <-  getForm("http://scholar.google.com/scholar", as_q =title, as_authors=authors, as_publication=journal,
  #               as_ylo=low.year, as_yhi=high.year, hl = 'en', btnG = 'Search',
  #               .opts = list(verbose = FALSE), curl = curl)  # suppressWarnings(sink( [] , file=NULL)) -- doesn't work
  z <- do.call(getForm, argh)
  
  dd = htmlParse(z)
  links = getNodeSet(dd, "//a[@href]")
  
  # do something to identify the link you want
  found <- FALSE
  ind <- 35
  while(!found){
    if (ind>length(links)){
      stop('No results found')
    }
    node.attr <- xmlAttrs(links[[ind]])
    if (any(names(node.attr)%in%'onclick')){
      info <- node.attr['onclick']
      found <- TRUE
    } else{
      ind <- ind + 1
    }
  }
  m <- regexpr("\'(.*?)\'", info)
  res <- regmatches(info, m)
  res <- substring(res, 2, nchar(res)-1)
  bib.url <- paste('http://scholar.google.com/scholar.bib?q=info:', res, 
                   ':scholar.google.com/&output=citation&hl=en&ct=citation&cd=0', sep='')
  print('Opening browser to Google Scholar BibTeX entry...')
  shell.exec(bib.url)
  
  # won't work and against Google terms of service
  # tmp = getURL(bib.url, curl = curl)
}
