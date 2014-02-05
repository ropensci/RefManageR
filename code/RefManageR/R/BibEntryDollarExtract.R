#' Extract fields from a BibEntry object
#' 
#' used to extract a single field from each entry in a BibEntry object
#' @param x - an object of class BibEntry
#' @param name - the field to extract
#' @S3method $ BibEntry
#' @note \code{name} may be \dQuote{bibtype} to extract entry types or \dQuote{key} to extract keys.
#' @return a named list of values for the field specified by name for each entry; \code{NULL} if the field is not present for
#' a particular entry.  The names attribute of the returned list contains the entry keys (potentially back-quoted).
#' @examples
#' #' file.name <- system.file("sampleData", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' bib[[50:55]]$author
#' bib[[seq_len(5)]]$bibtype
`$.BibEntry` <- function (x, name){
  if (!length(x)) 
    return(NULL)
  is_attribute <- name %in% bibentry_attribute_names
  rval <- if (is_attribute) 
    lapply(unclass(x), attr, name)
  else lapply(unclass(x), "[[", name)
  if (length(rval) == 1L) 
    return(rval[[1L]])
  setNames(rval, sapply(unclass(x), attr, "key"))
}