#' Flatten and unflatten BibEntry objects
#'
#' \code{RelistBibEntry} unflattens a BibEntry object that has been flattened with \code{unlist}.
#'
#' @param flesh list; an \code{unlist}ed BibEntry object
#' @param skeleton currently ignored
#' @details \code{RelistBibEntry} is only intended for use with \code{unlist}ed BibEntry objects.
#' @seealso \code{\link{as.BibEntry}}
#' @return \code{RelistBibEntry} - an object of class BibEntry
#' @rdname unlist.BibEntry
#' @export
#' @keywords database list manip
RelistBibEntry <- function(flesh, skeleton = NULL){
  attrs <- attributes(flesh)[bibentry_list_attribute_names]
  key.ind <- which(names(flesh) == 'key')
  type.ind <- which(names(flesh) == 'bibtype')
  N <- length(key.ind)
  res <- vector('list', N)
  dat <- NULL
  if (inherits(flesh[key.ind[1L]-1L][[1L]], 'POSIXct'))
    dat <- flesh[key.ind[1L]-1L][[1L]]
  res[[1L]] <- structure(flesh[seq_len(type.ind[1L]-1L)], bibtype=as.character(flesh[type.ind[1L]]),
                         dateobj = dat, key=as.character(flesh[key.ind[1L]]))
  if (N > 1L){
    for (i in 2L:N){
      dat <- NULL  # 'dateobj' may be missing
      if (inherits(flesh[key.ind[i]-1L][[1L]], 'POSIXct'))
        dat <- flesh[key.ind[i]-1L][[1L]]
      res[[i]] <- structure(flesh[(key.ind[i-1L]+1L):(type.ind[i]-1L)], bibtype= as.character(flesh[type.ind[i]]),
                            dateobj = dat, key = as.character(flesh[key.ind[i]]))
    }
  }
  attributes(res)[bibentry_list_attribute_names] <- attrs
  class(res) <- c('BibEntry', 'bibentry')
  res
}
