#' Extract all fields present in a BibEntry object
#' 
#' These functions return a list of all fields present in a BibEntry object.
#' @param x a BibEntry object.
#' @return a list with the same length as \code{x} of character vectors giving the 
#' fields present in each entry of a BibEntry object.
#' @note The only difference between \code{fields} and \code{levels} is that
#' \code{levels} returns a list with element names corresponding to entry keys.
#' @method levels BibEntry
#' @export
#' @keywords methods
#' @aliases fields
#' @examples 
#' bib <- as.BibEntry(list(c(bibtype = "Article", key = "mclean2014a", title = "My New Article", 
#'   author = "Mathew W. McLean", 
#'   journaltitle = "The Journal", date = "2014-01"), c(bibtype = "Book", key = "mclean2014b", 
#'   title = "My New Book", editor = "Mathew W. McLean", ISBN = "247123837", date = "2014-02")))       
#' fields(bib)
#' levels(bib)
levels.BibEntry <- function(x){
  if (!length(x))
    return(x)
  return(setNames(lapply(unclass(x), names), unlist(x$key)))
}

#' @rdname levels.BibEntry
#' @export
fields <- function(x){
  return(lapply(unclass(x), names))
}