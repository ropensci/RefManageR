`$.BibEntry` <- function (x, name){
  if (!length(x)) 
    return(NULL)
  is_attribute <- name %in% bibentry_attribute_names
  rval <- if (is_attribute) 
    lapply(unclass(x), attr, name)
  else lapply(unclass(x), "[[", name)
  if (length(rval) == 1L) 
    rval <- rval[[1L]]
  rval
}