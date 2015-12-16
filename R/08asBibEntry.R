#' Coerce to a BibEntry object
#'
#' Functions to check if an object is a BibEntry, or coerce it if possible.
#'
#' @param x any \code{R} object.
#' @details \code{as.BibEntry} is able to coerce suitably formatted character vectors, \code{\link{bibentry}} objects, lists,
#' and data.frames to BibEntry objects.  See the examples.
#' @note Each entry to be coerced should have a bibtype, key, and all required fields for the specified bibtype.
#' @return \code{as.BibEntry} - if successful, an object of class BibEntry.
#' @aliases is.BibEntry
#' @keywords utilities
#' @export
#' @seealso \code{\link{BibEntry}}
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))[[20:21]]
#' identical(as.BibEntry(unlist(bib)), bib)  ## see also RelistBibEntry
#'
#' identical(as.BibEntry(unclass(bib)), bib)
#'
#' identical(as.BibEntry(as.data.frame(bib)), bib)
#'
#' bib <- c(bibtype = "article", key = "mclean2014", title = "My New Article",
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01")
#' as.BibEntry(bib)
#'
#' bib <- bibentry(bibtype = "article", key = "mclean2014", title = "My New Article",
#' journal = "The Journal", year = 2014, author = "Mathew W. McLean")
#' print(bib, .bibstyle = "JSS")
#' as.BibEntry(bib)
#'
#' bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
#'   c(bibtype = "article", key = "mclean2014b", title = "Newer Article",
#'   author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
#' as.BibEntry(bib)
as.BibEntry <- function(x){
  if (!length(x))
    return(x)
  if (inherits(x, 'BibEntry')){
    class(x) <- c('BibEntry', 'bibentry')
  }else if (inherits(x, 'bibentry')){
    att <- attributes(x)
    x <- lapply(unclass(x), function(y){
      attr(y, "dateobj") <- ProcessDates(y)
      if (!length(attr(y, "key")))
        attr(y, "key") <- CreateBibKey(y[['title']], y[['author']], y[['year']])
      check <- try(.BibEntryCheckBibEntry1(y), TRUE)
      if (inherits(check, 'try-error')){
        message(paste0('Ignoring entry titled \"', y[['title']], '\" because ', strsplit(check, '\\n[[:space:]]*')[[1]][2]))
        return(NULL)
      }
      y
    })
    x <- x[!sapply(x, is.null)]
    if (length(x)){
      attributes(x) <- att
      class(x) <- c('BibEntry', 'bibentry')
    }
  }else if (is.character(x)){

    if (is.na(x['bibtype']) || is.na(x['key']))
      stop("Object of class character must have entries named bibtype and key.")
    x <- as.list(x)
    attr(x, 'entry') <- x$bibtype
    attr(x, 'key') <- x$key
    x$bibtype <- NULL
    x$key <- NULL
    x <- MakeBibEntry(x, FALSE)
  }else if(is.data.frame(x)){
    .fields <- colnames(x)
    if (is.null(x$bibtype))
      stop("data.frame must have column for 'bibtype'.")
    keys <- rownames(x)
    if (keys[1L] == '1')
      warning('rownames of data.frame not meaningful for creating keys')

    y <- vector('list', length(x))
    for (i in seq_len(nrow(x))){
      na.ind <- which(!is.na(x[i, ]))
      y[[i]] <- as.BibEntry(c(setNames(as.character(x[i, na.ind]), .fields[na.ind]), key = keys[i]) )
    }
    y <- MakeCitationList(y)
    return(y)

  }else if(is.list(x)){
    if(length(x) == 1L && !is.null(attr(x, 'bibtype'))){
        class(x) <- c('BibEntry', 'bibentry')
    }else if (!is.null(x$dateobj)){  # x has been unlist'ed
        x <- RelistBibEntry(x)
    }else if (!is.null(attr(x[[1L]], 'bibtype'))){  # x simply unclass'ed
        class(x) <- c('BibEntry', 'bibentry')
    }else{
      if (length(x[[1L]]) == 1L){
        x <- do.call(BibEntry, x)
      }else{
        x <- sapply(x, function(...) do.call(BibEntry, as.list(...)))
        class(x) <- c("BibEntry", "bibentry")
      }
    }
  }else{
    stop(paste0("Cannot coerce object of class '", class(x), "' to BibEntry"))
  }
  x <- MakeKeysUnique(x)
  return(x)
}

#' @rdname as.BibEntry
#' @return \code{is.BibEntry} - logical; \code{TRUE} if \code{x} is a BibEntry object.
#' @export
is.BibEntry <- function(x){
  inherits(x, "BibEntry")
}
