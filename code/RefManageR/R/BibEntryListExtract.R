#' Extract entries from a BibEntry object by index
#' 
#' Operator for extracting BibEntry objects by index.
#' 
#' @param x a BibEntry object
#' @param i numeric indices of entries to exctract, or a character vector of keys corresponding to the entries to be
#' extracted.
#' @param drop logical, should attributes besides class be dropped from result?
#' @method [[ BibEntry
#' @export
#' @note This method is different than the usual operator \code{[[} for lists in that a vector of indices may be specified.
#' 
#' This method behaves differently than the \code{[} operator for BibEntry objects in that it does not expand
#' crossreferences when returning, so that a parent entry or xdata entry will be dropped if it is not also indexed
#' when indexing the child entry.
#' 
#' This method is not affected by the value of \code{BibOptions()$return.ind}.
#' @return an object of class BibEntry.
#' @keywords database manip list
#' @family operators
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' bib[[20:21]]
#' bib[c("hyman", "loh")]
#' 
#' ## Note this is FALSE because [[ does not inherit from the dropped parent entry while [ does.
#' identical(bib[1], bib[[1]])
`[[.BibEntry` <- function (x, i, drop = FALSE){
  if (!length(x)) 
    return(x)
  cl <- class(x)
  class(x) <- NULL
  if (is.character(i) && is.null(names(x))) 
    names(x) <- .BibEntry_get_key(x)
  y <- x[i]
  if (!all(ok <- sapply(y, length) > 0L)) {
    warning("subscript out of bounds")
    y <- y[ok]
  }
  if (!drop) 
    attributes(y) <- attributes(x)[bibentry_list_attribute_names]
  class(y) <- cl
  y
}