names.BibEntry <- function(x){
  if(length(x)==1){
    return(names(unclass(x)[[1]]))
  }else{
    return(sapply(unclass(x), function(x) return(attr(x, 'key'))))
  }
}