`[[.BibEntry` <- function (x, i, drop = TRUE){
  if (!length(x)) 
    return(x)
  cl <- class(x)
  class(x) <- NULL
  if (is.character(i) && is.null(names(x))) 
    names(x) <- .bibentry_get_key(x)
  y <- x[i]
  if (!all(ok <- sapply(y, length) > 0L)) {
    warning("subscript out of bounds")
    y <- y[ok]
  }
  if (!drop) 
    attributes(y) <- attributes(x)[bibentry_list_attribute_names]
  class(y) <- cl
  y
}