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