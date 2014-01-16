relist.BibEntry <- function(flesh, skeleton = NULL){
  key.ind <- which(names(flesh) == 'key')
  N <- length(key.ind)
  res <- vector('list', N)
  res[[1L]] <- structure(flesh[seq_len(key.ind[1L]-3L)], bibtype=as.character(flesh[key.ind[1L]-2L]), 
                         dateobj =flesh[key.ind[1L]-1L][[1L]], key=as.character(flesh[key.ind[1L]]))
  if (N > 1L){
    for (i in 2L:N){
      res[[i]] <- structure(flesh[(key.ind[i-1L]+1L):(key.ind[i]-3L)], bibtype= as.character(flesh[key.ind[i]-2L]),
                            dateobj = flesh[key.ind[i]-1L][[1L]], key = as.character(flesh[key.ind[i]]))
    }
  }
  class(res) <- c('BibEntry', 'bibentry')
  res
}