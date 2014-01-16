`$<-.BibEntry` <- function(x, name, value){
  stopifnot(length(x) == length(value) || length(value) <= 1)
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
        if (name %in% .BibEntryNameList) 
          ArrangeAuthors(value[[i]])
        else paste(value[[i]])
      }
      if ( name %in% .BibEntryDateField){  # dateobj may need to be updated
        tdate <- ProcessDates(x)
        if (!(is.null(tdate) || inherits(tdate, 'try-error') || is.na(tdate)))
          attr(res, 'dateobj') <- tdate
      }
    }
  }
 
  for (i in seq_along(x)) .BibEntryCheckBibEntry1(x[[i]])
  class(x) <- c("BibEntry", "bibentry")
  x
}