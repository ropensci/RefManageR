#' Names (keys) of a BibEntry object
#' 
#' Functions to get and set the keys of an object of class BibEntry
#' @aliases names<-.BibEntry
#' @rdname names.BibEntry
#' @method names BibEntry
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @param x an object of class BibEntry
#' @return \code{names} - character vector of the keys of the BibEntry object.
#' @keywords attribute
#' @method names BibEntry
#' @export
#' @examples
#' bib <- ReadBib(system.file("Bib", "test.bib", package = "RefManageR"))
#' names(bib)
#' names(bib)[1] <- 'newkey'
names.BibEntry <- function(x){
  return(unlist(x$key))  # return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
}
