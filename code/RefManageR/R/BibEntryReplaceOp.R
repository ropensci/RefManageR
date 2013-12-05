`[<-.BibEntry` <- function(x, i, j, ..., value){
  if (missing(i)){
    y <- x
  }else if (missing(j)){
    y <- unclass(x[i])
  }else{
    y <- unclass(x[i, j, ...])
  }
  return(y)
  if (inherits(value, 'BibEntry')){
    value <- unclass(value)
    
  }else{
    value <- as.list(value)
    N <- length(value)
    if (!length(y))
      stop('No elements to replace.')
    
    fields <- names(value)
    if (is.null(fields) || any(fields == ''))
      stop('All values in replacement must have a name corresponding to BibTeX field.')
    
    if (length(y))
    browser()
  }
  if (!is.null(value) && N %% length(x) == 0){
    warning(paste0('BibEntry object length is not a multiple of replacement length'))
  }
  value <- rep(value, l = N)
  
  for (k in seq_along(i))
    y[[i[k]]] <- value[k]
  
  class(y) <- c('BibEntry', 'bibentry')
  return(y)
}