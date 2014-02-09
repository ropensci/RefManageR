#' @description \code{unlist} flattens a BibEntry object to a single list where every field (including \code{bibtype} and \code{key})
#' of every entry is a separate element in the list.
#' @param x a BibEntry object to flatten
#' @param recursive ignored.
#' @param use.names ignored.
#' @method unlist BibEntry
#' @export
#' @return For \code{unlist}, a list with bib entries collapsed into a single list.
#' @note The names of the list elements from an unlisted BibEntry object will not be unique.  To do this see \code{\link{make.unique}}.
#' @aliases relist.BibEntry
#' @rdname unlist.BibEntry
#' @examples 
#' bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article", 
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"), 
#'   c(bibtype = "article", key = "mclean2014b", title = "My Newer Article", 
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))       
#' bib <- as.BibEntry(bib)
#' unlist(bib)
#' relist.BibEntry(unlist(bib))
unlist.BibEntry <- function(x, recursive = FALSE, use.names = TRUE){
  x <- lapply(unclass(x), function(x){
    x$bibtype <- attr(x, 'bibtype')
    x$dateobj <- attr(x, 'dateobj')
    x$key <- attr(x, 'key')
    x
  })
  unlist(x, FALSE)
  # class(x) <- c('character') specifying character breaks author
  # x
}