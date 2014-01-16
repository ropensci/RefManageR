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