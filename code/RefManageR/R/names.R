names.BibEntry <- function(x){
  return(unlist(x$key))  # return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
}

#`names<-.BibEntry` <- function(x, value){
#    res <- `names<-`(unclass(x), value) 
#    class(res) <- c('BibEntry', 'bibentry')
#}

