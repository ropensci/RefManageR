#' Search BibEntry objects by field
#' 
#' Allows for searching and indexing a BibEntry object by fields, including names and dates.  The extraction operator and 
#' the SearchBib function simplying provide different interfaces to the same search functionality.  
#' @param x - an object of class BibEntry
#' @param use.regex - boolean; are the search terms regular expressions or should exact matching be used?
#' @param ignore.case - boolean; should case be ignored when comparing strings?
#' @param match.date - how should the date fields date, urldate, eventdate, and origdate.  Default is \dQuote{year.only}, so 
#' that months and days in dates are ignored when comparing.  Currently, specifying any other value results the full date being
#' used.  See the Note section.
#' @param ... - arguments in the form \code{bib.field = search.term}.  For \code{SearchBib}, can alternatively have same
#' form as \code{i}.
#' @aliases [.BibEntry
#' @keywords database manip list
SearchBib <- function(x, use.regex = TRUE, ignore.case = TRUE, match.date = .BibOptions$match.date, 
                      match.author = .BibOptions$match.author, return.index = .BibOptions$return.ind, ...){
  bibopts <- .BibOptions$copy()
  .BibOptions$use.regex <- use.regex
  .BibOptions$ignore.case <- TRUE
  .BibOptions$match.date <- match.date
  .BibOptions$match.author <- match.author
  .BibOptions$return.ind <- return.index
  x <- do.call(`[.BibEntry`, list(x, ...))
  .BibOptions <- bibopts$copy()
  return(x)
}