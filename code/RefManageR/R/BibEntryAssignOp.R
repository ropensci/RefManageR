#' Replace values for a particular field in a BibEntry object
#' 
#' Used to replace the values stored for a specified field in a BibEntry object.
#' @param x a BibEntry object
#' @param name string; the field to assign the new values to.
#' @param value character vector; the replacement field values to be assigned.
#' @return an object of class BibEntry with the updated fields.
#' @note The method expects date and name list fields to be in the format expected by Biblatex.  The 
#' field specified by \code{name} does not have to be one currently in \code{x}.
#' @method $<- BibEntry
#' @export
#' @family operators
#' @keywords methods
#' @examples
#' bib <- BibEntry(bibtype = "misc", key = "mclean", author = "Mathew W. McLean", 
#'   title = "My Work", year = "2012")
#' bib$year <- 2014
#' bib$author <- "McLean, M. W. and Carroll, R. J." 
#' bib$url <- "http://example.com"
#' bib
#' 
#' bib <- c(bib, as.BibEntry(citation()))
#' bib[1]$author[2] <- person(c("Raymond", "J."), "Carroll")
#' bib$author
`$<-.BibEntry` <- function(x, name, value){
  # browser()
  stopifnot(length(x) == length(value) || length(value) <= 1 || name %in% .BibEntryNameList)
  is_attribute <- name %in% bibentry_attribute_names
  x <- unclass(x)
  name <- tolower(name)
  if (length(value) <= 1)
    value <- rep(.listify(value), length.out = length(x))
  if (name == "bibtype") {
    stopifnot(all(sapply(value, length) == 1L))
    BibTeX_names <- names(BibLaTeX_entry_field_db)
    value <- unlist(value)
    pos <- match(tolower(value), tolower(BibTeX_names))
    if (any(is.na(pos))) 
      stop(gettextf("%s has to be one of %s", sQuote("bibtype"), 
                    paste(BibTeX_names, collapse = ", ")), domain = NA)
    value <- as.list(BibTeX_names[pos])
  }
  for (i in seq_along(x)) {
    if (is_attribute) {
      attr(x[[i]], name) <- if (is.null(value[[i]])) 
        NULL
      else paste(value[[i]])
    }else {
      x[[i]][[name]] <- if (is.null(value[[i]])) 
        NULL
      else {
        if (name %in% .BibEntryNameList){
          if (inherits(value, "person")){
            value
          }else{
            ArrangeAuthors(value[[i]])  
          }
        }else paste(value[[i]])
      }
      if ( name %in% .BibEntryDateField){  # dateobj may need to be updated
        tdate <- ProcessDates(x[[i]])
        if (is.null(tdate))
          stop(paste0('The specified Date Field value is not in a valid format for Bibtex/Biblatex'))
        attr(x[[i]], 'dateobj') <- tdate
      }
    }
  }
 
  for (i in seq_along(x)) .BibEntryCheckBibEntry1(x[[i]])
  class(x) <- c("BibEntry", "bibentry")
  x
}