#' @param value character vector of new key values to replace into \code{x} 
#' @rdname names.BibEntry
#' @method names<- BibEntry
#' @export
#' @return \code{names<-} the updated BibEntry object.
`names<-.BibEntry` <- function(x, value){
  x <- mapply(`attr<-`, unclass(x), list('key'), as.list(value))
  class(x) <- c('BibEntry', 'bibentry')
  x
}

# `names<-.BibEntry` <- function(x, value){
#   x <- unlist(x)
#   x[names(x)=='key'] <- value
#   return(relist.BibEntry(x))
# }

# `names<-.BibEntry` <- function(x, value){
#   x$key <- value
#   x
# }