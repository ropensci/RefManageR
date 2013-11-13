unlist.BibEntry <- function(x, recursive = FALSE, use.names = TRUE){
  x <- lapply(unclass(x), function(x){
    x$bibtype <- attr(x, 'bibtype')
    x$key <- attr(x, 'key')
    x
  })
  x <- unlist(x, FALSE)
  # class(x) <- c('character') specifying character breaks author
  x
}