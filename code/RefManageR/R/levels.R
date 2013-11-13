fields <- levels.BibEntry <- function(x){
  return(lapply(unclass(x), names))
}