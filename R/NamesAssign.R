#' @param value character vector of new key values to replace into \code{x} 
#' @rdname names.BibEntry
#' @method names<- BibEntry
#' @export
#' @return \code{names<-} the updated BibEntry object.
`names<-.BibEntry` <- function(x, value){
  x <- mapply(`attr<-`, unclass(x), list('key'), as.list(value), SIMPLIFY = FALSE)
  class(x) <- c('BibEntry', 'bibentry')
  x <- MakeKeysUnique(x)
  x
}
