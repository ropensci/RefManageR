relist.BibEntry <- function(flesh, skeleton=NULL){
  #  browser()
  key.ind <- which(names(flesh)=='key')
  N <- length(key.ind)
  res <- vector('list', N)
  res[[1]] <- structure(flesh[1:(key.ind[1]-2)], bibtype=as.character(flesh[key.ind[1]-1]), 
                        key=as.character(flesh[key.ind[1]]))
  if(N>1){
    for(i in 2:N){
      res[[i]] <- structure(flesh[(key.ind[i-1]+1):(key.ind[i]-2)], bibtype= as.character(flesh[key.ind[i]-1]),
                            key = as.character(flesh[key.ind[i]]))
    }
  }
  class(res) <- c('BibEntry', 'bibentry')
  res
}