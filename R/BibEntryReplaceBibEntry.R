#' Assign a BibEntry entry to another BibEntry object
#' 
#' Replace one entry in a BibEntry object with another
#' @param x - a BibEntry object
#' @param i - a numeric index or a string entry key
#' @param value - a single entry BibEntry object or an object that can be 
#' coereced to BibEntry using \code{\link{as.BibEntry}}
#' @return an object of class BibEntry
#' @details
#' This function will replace the specified entry in \code{x} with the entry given
#' by \code{value}.  To replace multiple entries see \code{\link{[<-.BibEntry}}.
#' @method [[<- BibEntry
#' @export
#' @keywords methods
#' @family operators
`[[<-.BibEntry` <- function(x, i, value){
  value <- as.BibEntry(value)
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
  x <- MakeKeysUnique(x)
  return(x)
}
