c.BibEntry <- function (..., recursive = FALSE){
    args <- list(...)
    if (!all(sapply(args, inherits, "bibentry"))) 
        warning(gettextf("method is only applicable to %s objects", 
            sQuote("bibentry")), domain = NA)
    args <- lapply(args, unclass)
    rval <- do.call("c", args)
    class(rval) <- c("BibEntry", "bibentry")
    rval
}