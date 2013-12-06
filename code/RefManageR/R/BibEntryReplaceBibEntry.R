`[[<-.BibEntry` <- function(x, i, value){
  if (!inherits(value, 'BibEntry') || length(value) != 1 || length(i) != 1)
    stop('Object to replace must be a bibentry or BibEntry object of length 1.')
  
  if (is.character(i)){
    if (!any(i <- i == names(x)))
      stop('index is not numeric or a valid key of the BibEntry object') 
  }else if (!is.numeric(i)){
    stop('index is not numeric or a valid key of the BibEntry object')
  }
  
  x <- unclass(x)
  value <- unclass(value)
  x[i] <- value
  class(x) <- c('BibEntry', 'bibentry')

  return(x)
}