toBiblatex <- function(object, ...){
    format_bibentry1 <- function(object) {
      object <- unclass(object)[[1L]]
      rval <- paste0("@", attr(object, "bibtype"), "{", attr(object, 
          "key"), ",")
      nl.ind <- which(names(object) %in% .BibEntryNameList)
      for (i in nl.ind)
        object[i] <- format_author(object[[i]])
      rval <- c(rval, sapply(names(object), function(n) paste0("  ", 
          n, " = {", object[[n]], "},")), "}", "")
      return(rval)
    }
    if (length(object)) {
        object$.index <- NULL
        rval <- head(unlist(lapply(object, format_bibentry1)), 
            -1L)
    }
    else rval <- character()
    class(rval) <- "Bibtex"
    rval
}