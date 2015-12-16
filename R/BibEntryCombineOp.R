#' Combine BibEntry objects.
#'
#' Combines mutliple BibEntry objects into a single one.
#' @param ... - BibEntry objects to be concatenated.
#' @param recursive - logical; ignored.
#' @return a single BibEntry object.
#' @method c BibEntry
#' @export
#' @note \code{c} will remove all attributes besides \code{class}.
#'
#' No checking for duplicate entries is performed though keys will be made unique.
#' @keywords methods
#' @family operators
#' @examples
#' bib <- c(BibEntry(bibtype = "article", key = "mclean2014a", title = "My New Article",
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
#'   BibEntry(bibtype = "article", key = "mclean2014b",
#'   title = "My Newer Article", author = "Mathew W. McLean", journaltitle = "The Journal",
#'   date = "2014-02"))
c.BibEntry <- function (..., recursive = FALSE){
    args <- list(...)
    if (!all(sapply(args, inherits, "bibentry")))
        warning(gettextf("method is only applicable to %s objects",
            sQuote("BibEntry")), domain = NA)
    args <- lapply(args, unclass)
    rval <- do.call("c", args)
    class(rval) <- c("BibEntry", "bibentry")
    rval <- MakeKeysUnique(rval)
    rval
}
