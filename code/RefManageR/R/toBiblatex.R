#' Convert BibEntry objects to BibTeX or BibLaTeX
#' 
#' toBiblatex converts a BibEntry object to character vectors with BibLaTeX markup.  toBibtex will convert a BibEntry object
#' to character vectors with BibTeX markup, converting some BibLaTeX fields and all entry types that are not supported 
#' by BibTeX to ones that are supported.  
#' 
#' @aliases toBibtex.BibEntry
#' @S3method toBiblatex BibEntry
toBiblatex <- function(object, ...){
    format_bibentry1 <- function(object) {
      object <- unclass(object)[[1L]]
      rval <- paste0("@", attr(object, "bibtype"), "{", attr(object, 
          "key"), ",")
      nl.ind <- which(names(object) %in% .BibEntryNameList)
      for (i in nl.ind)
        object[i] <- encoded_text_to_latex(format_author(object[[i]]), "UTF-8")
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