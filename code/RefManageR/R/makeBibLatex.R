#####################################################################################
## FIELDS: 
#####################################################################################
fmtDate <- function(paper){
  if (is.null(paper$dateobj)){
    if (!length(year)) 
      year <- "????"
    paste0("(", collapse(year), ")")
  }else{
    .mon <- attr(paper$dateobj, 'month')
    .day <- attr(paper$dateobj, 'day')
    if (is.interval(paper$dateobj)){
      begind <- int_start(paper$dateobj)
      endd <- int_end(paper$dateobj)
      paste0(format(begind, '%b %d, %Y'), format(endd, '%b %d, %Y'), collapse =)
    }else{
      
    }
  }
}

fmtDOI <- function(s){
  if (length(s) && .BibOptions$print.doi)
    paste0('DOI: ', s)
}

fmtPages <- function(s){
  if (length(s)) 
    style(paste0(prefix, collapse(s), suffix))
}

fmtJournal <- function(s){
  if (!is.null(s$journaltitle)){
    paste0('In: ', emph(cleanupLatex(s$journaltitle)))
  }else if(!is.null(s$journal)){
    paste0('In: ', emph(cleanupLatex(s$journal)))  
  }
}
  
fmtURL <- function(paper){
  if (length(paper$url)){
    if (length(paper$urldate)){
      fDate <- format()
    }else{
      
    }
  }
}
##################################################################
## Formatting functions

fmtPrefix <- function(paper){
  if (.BibOptions$bib.prefix == 'numeric'){
    paste0("[", paper$.index, "]")
  }else{
    NULL
  }
}

collapse <- function (strings){ 
  paste(strings, collapse = "\n")
}

labelclean <- function (prefix = NULL, suffix = NULL, style = plain){
    f <- label(prefix, suffix, style)
    function(s) f(cleanupLatex(s))
}

label <- function (prefix = NULL, suffix = NULL, style = plain){
    force(prefix)
    force(suffix)
    force(style)
    function(s) if (length(s)) 
        style(paste0(prefix, collapse(s), suffix))
}


emphclean <- function (s){
  emph(cleanupLatex(s))
}

emph <- function (s){
  if (length(s)) 
    paste0("\\emph{", collapse(s), "}")
}

plainclean <- function (s){
  plain(cleanupLatex(s))
}

plain <- function (pages){
  if (length(pages)) 
    collapse(pages)
}

sentence <- function (..., sep = ", "){
    strings <- c(...)
    if (length(strings)) {
        addPeriod(paste(strings, collapse = sep))
    }
}

addPeriod <- function (string){ 
  sub("([^.?!])$", "\\1.", string)
}

#####################################################################################
## Entry types: Bibliography Drivers in BibLaTeX (Sec. 4.2.3 in manual)
#####################################################################################
formatArticle <- function(paper){
    collapse(c(fmtPrefix(paper), sentence(authorList(paper), 
        fmtYear(paper$year), sep = " "), fmtTitle(paper$title), 
        sentence(fmtJournal(paper), volNum(paper), fmtPages(paper$pages)), 
        sentence(fmtISSN(paper$issn), extraInfo(paper))))
}