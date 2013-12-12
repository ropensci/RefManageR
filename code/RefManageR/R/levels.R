#' 
levels.BibEntry <- function(x){
  return(setNames(lapply(unclass(x), names), unlist(x$key)))
}

fields <- function(x){
  return(lapply(unclass(x), names))
}