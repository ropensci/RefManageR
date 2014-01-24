relist.BibEntry <- function(flesh, skeleton = NULL){
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
  class(res) <- c('BibEntry', 'bibentry')
  res
}