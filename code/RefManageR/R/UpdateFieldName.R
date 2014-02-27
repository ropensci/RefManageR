#' Rename a field in a BibEntry object.
#' 
#' This function will rename a field, in every entry where it is present, in a 
#' BibEntry object.
#' @param x - a BibEntry object
#' @param old.field - string; the current name of the field to be renamed
#' @param new.field - string; the new name to replace \code{old.field}
#' @return \code{x}, with the renamed field.
#' @keywords manip utilities
#' @export
#' @examples
#' bib <- as.BibEntry(list(c(bibtype = "article", key = "mclean2014a", title = "My New Article", 
#'   author = "Mathew W. McLean", journal = "The Journal", date = "2014-01"), 
#'   c(bibtype = "article", key = "mclean2014b", title = "My Newer Article", 
#'     author = "Mathew W. McLean", journal = "The Journal", date = "2014-02")))       
#' bib <- UpdateFieldName(bib, "journal", "journaltitle")
#' toBiblatex(bib)   
UpdateFieldName <- function(x, old.field, new.field){
  if (!length(x))
    return(x)
  # stopifnot(length(new.field) == 1L)
  x <- unlist(x)
  names(x)[names(x) %in% old.field] <- new.field
  RelistBibEntry(x)
}