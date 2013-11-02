names.BibEntry <- function(x){
  return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
}

#`names<-.BibEntry` <- function(x, value){
#    res <- `names<-`(unclass(x), value) 
#    class(res) <- c('BibEntry', 'bibentry')
#}

fields <- levels.BibEntry <- function(x){
  return(lapply(unclass(x), names))
}

UpdateFieldName <- function(x, old.field, new.field){
  browser()
  x <- unlist(x)
  names(x)[names(x)==old.field] <- new.field
  x <- relist.BibEntry(x)
  x
}

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
