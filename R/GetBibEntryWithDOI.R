#' Lookup a Bibtex entry using a Digital Object Identifier
#'
#' Uses the DOI System API to look up bibliography information given a set of DOIs.
#'
#' @param doi character vector; DOIs to use to retrieve bibliographic information.
#' @param temp.file string; a file to write the Bibtex data returned by the
#' DOI System to.
#' @param delete.file logical; should \code{temp.file} be deleted when the
#' function exits?
#' @return an object of class BibEntry.
#' @export
#' @details
#' The bibliographic information returned by the search of the \url{http://dx.doi.org}
#' API is temporarily
#' written to a file and then read back into \code{R} and return as a
#' \code{BibEntry} object.
#' @references \url{http://www.doi.org/tools.html}
#' @importFrom httr http_error GET content http_error modify_url
#' @importFrom utils URLdecode
#' @seealso \code{\link{ReadCrossRef}}, \code{\link{BibEntry}}
#' @examples
#' if (interactive() && !httr::http_error("http://dx.doi.org/"))
#'   GetBibEntryWithDOI(c("10.1016/j.iheduc.2003.11.004", "10.3998/3336451.0004.203"))
GetBibEntryWithDOI <- function(doi, temp.file=tempfile(fileext = '.bib'),
                               delete.file = TRUE){
  file.create(temp.file)
  on.exit(if (delete.file && file.exists(temp.file)) file.remove(temp.file))
  successes <- logical(length(doi))
  for (i in seq_along(doi)){
    temp <- GET(modify_url('http://dx.doi.org/', path = doi[i]),
                    config = list(followlocation = TRUE),
                      add_headers(Accept = "application/x-bibtex"))
    if (!http_error(temp)){
      temp <- content(temp, as = "text", encoding = "UTF-8")
      successes[i] <- TRUE
      ## if (is.raw(temp))
      ##   temp <- rawToChar(temp)
      write(temp, file = temp.file, append=TRUE)
    }
  }
  if (!all(successes)){
    failures <- paste(sQuote(doi[!successes]), collapse = ", ")
    message(gettextf("unable to retrieve bibliographic data for %s%s",
                     "the following supplied DOIs: ",
                     failures))
  }
  if (any(successes)){
    bib.res <- try(ReadBib(file=temp.file, .Encoding='UTF-8'), TRUE)

    if (inherits(bib.res, "try-error"))
      stop(gettextf("failed to parse the returned BibTeX results; if %s %s%s",
                    sQuote("delete.file"),
                    "is FALSE, you can try viewing and editing the file: ",
                    temp.file))

    bib.res$url <- vapply(bib.res$url, function(x) if (!is.null(x))
                                                 URLdecode(x), "") 

    return(bib.res)
  }
  return()  # No results
}
