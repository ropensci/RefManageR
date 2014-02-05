#' Search BibEntry objects by field
#' 
#' Allows for searching and indexing a BibEntry object by fields, including names and dates.  The extraction operator and 
#' the SearchBib function simplying provide different interfaces to the same search functionality.  
#' @param x - an object of class BibEntry
#' @param .opts - list of search options with name = value entries.  The following values may be set
#' \itemize{
#' \item use.regex - boolean; are the search terms regular expressions or should exact matching be used?
#' \item ignore.case - boolean; should case be ignored when comparing strings?
#' \item match.date - how should the date fields date, urldate, eventdate, and origdate.  Default is \dQuote{year.only}, so 
#' that months and days in dates are ignored when comparing.  Currently, specifying any other value results the full date being
#' used.  See the Note section.
#' \item match.author - character string; how should name fields be searched? If \dQuote{family.only}, only family names are
#' compared; if \dQuote{family.with.initials}, family name and given name initials are used; if \dQuote{exact}, full 
#' names  are used.
#' \item return.ind - boolean; if TRUE the returned object is numeric indices of match locations; otherwise, a BibEntry
#' object is returned
#' }
#' @param ... - arguments in the form \code{bib.field = search.term}.  For \code{SearchBib}, can alternatively have same
#' form as \code{i}.
#' @aliases [.BibEntry
#' @keywords database manip list
SearchBib <- function(x, .opts = list(), ...){
  
  if (length(.opts)){
    bibopts <- BibOptions(names(.opts)) 
    BibOptions(.opts)
  }
  x <- do.call(`[.BibEntry`, list(x, ...))
  BibOptions(bibopts)  # assign(.BibOptions, bibopts$copy(), "package:RefManageR")
  return(x)
}