`names<-.BibEntry` <- function(x, value){
  x <- unlist(x)
  x[names(x)=='key'] <- value
  return(relist.BibEntry(x))
}