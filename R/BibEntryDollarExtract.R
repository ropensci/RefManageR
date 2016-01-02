#' Extract fields from a BibEntry object
#' 
#' used to extract a single field from each entry in a BibEntry object
#' @param x an object of class BibEntry
#' @param name the field to extract
#' @method $ BibEntry
#' @export
#' @family operators
#' @note \code{name} may be \dQuote{bibtype} to extract entry types or \dQuote{key} to extract keys.
#' @return a named list of values for the field specified by name for each entry; \code{NULL} if the field is not present for
#' a particular entry.  The names attribute of the returned list contains the entry keys (potentially back-quoted).
#' @importFrom stats setNames
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' bib[[50:55]]$author
#' bib[[seq_len(5)]]$bibtype
`$.BibEntry` <- function (x, name){
  if (!length(x)) 
    return(NULL)
  is_attribute <- name %in% bibentry_attribute_names
  if (is_attribute){ 
    res <- lapply(unclass(x), attr, name)
  }else if (pmatch(name, "journaltitle", 0L)){
    res <- lapply(unclass(x), "[[", name = "journal", exact = FALSE)
  }else{
    res <- lapply(unclass(x), "[[", name)
  }
  if (length(res) == 1L) 
    return(res[[1L]])
  setNames(res, sapply(unclass(x), attr, "key"))
}
