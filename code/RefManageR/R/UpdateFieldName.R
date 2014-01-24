UpdateFieldName <- function(x, old.field, new.field){
  # browser()
  x <- unlist(x)
  names(x)[names(x)==old.field] <- new.field
  relist.BibEntry(x)
}