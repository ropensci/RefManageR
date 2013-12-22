toBiblatex.BibEntry <- function(object, ...) 
{
    format_author <- function(author) paste(sapply(author, function(p) {
        fnms <- p$family
        only_given_or_family <- is.null(fnms) || is.null(p$given)
        fbrc <- if (length(fnms) > 1L || any(grepl("[[:space:]]", 
            fnms)) || only_given_or_family) 
            c("{", "}")
        else ""
        gbrc <- if (only_given_or_family) 
            c("{", "}")
        else ""
        format(p, include = c("given", "family"), braces = list(given = gbrc, 
            family = fbrc))
    }), collapse = " and ")
    format_bibentry1 <- function(object) {
      browser()
        object <- unclass(object)[[1L]]
        rval <- paste0("@", attr(object, "bibtype"), "{", attr(object, 
            "key"), ",")
        if ("author" %in% names(object)) 
            object$author <- format_author(object$author)
        if ("editor" %in% names(object)) 
            object$editor <- format_author(object$editor)
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