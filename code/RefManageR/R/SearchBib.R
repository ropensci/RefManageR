#' @param .opts list of search options with \code{name = value} entries.  Any option described 
#' in \code{\link{BibOptions}} is valid, with the following being the most relevant ones
#' \itemize{
#' \item \code{use.regex} - logical; are the search terms regular expressions or should exact matching be used?
#' \item \code{ignore.case} - logical; should case be ignored when comparing strings?
#' \item \code{match.date} - how should the date fields date, urldate, eventdate, and origdate.  Default is \dQuote{year.only}, so 
#' that months and days in dates are ignored when comparing.  Currently, specifying any other value results the full date being
#' used.  See the Note section.
#' \item \code{match.author} - character string; how should name fields be searched? If \dQuote{family.only}, only family names are
#' compared; if \dQuote{family.with.initials}, family name and given name initials are used; if \dQuote{exact}, full 
#' names  are used.
#' \item \code{return.ind} - logical; if TRUE the returned object is numeric indices of match locations; otherwise, a BibEntry
#' object is returned
#' }
#' @aliases [.BibEntry
#' @export
#' @rdname SearchBib
SearchBib <- function(x, .opts = list(), ...){
  if (length(.opts)){
    oldopts <- BibOptions(.opts)
    on.exit(BibOptions(oldopts))
  }
  x <- do.call(`[.BibEntry`, list(x, ...))
  return(x)
}