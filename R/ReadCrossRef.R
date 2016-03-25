#' Search CrossRef for citations.
#'
#' Provides an interface to the CrossRef API, searching for citations given a string query.  Results are written to a
#' bib file, read back into \code{R} using \code{\link{WriteBib}}, and returned as a BibEntry object.
#'
#' @param query string; search term
#' @param limit numeric; maximum number of entries to return
#' @param sort string; how should the results from CrossRef be returned.
#' @param year numeric; if specified, only results from this year will be returned.
#' @param min.relevance numeric; only results with a CrossRef-assigned relevance score at least this high will be returned.
#' @param temp.file string; file name to use for storing Bibtex information returned by CrossRef.
#' @param delete.file boolean; should the bib file be deleted on exit?
#' @param verbose boolean; if \code{TRUE}, additional messages are output regarding the results of the query.
#' @return An object of class BibEntry.
#' @details CrossRef assigns a score between 0 and 100 based on how relevant a reference seems to be
#' to your query.  The API documentation warns that while false negatives are unlikely, the search can be prone
#' to false positives.  Hence, setting \code{min.revelance} to a high value may be necessary.
#' @importFrom RJSONIO fromJSON
#' @importFrom RCurl getForm getURLContent url.exists
#' @export
#' @keywords database
#' @seealso \code{\link{ReadZotero}}, \code{\link{BibEntry}}, \code{\link{GetDOIs}}
#' @family pubmed
#' @references \url{http://search.crossref.org/help/api}
#' @examples
#' if (interactive() && url.exists("http://search.crossref.org/")){
#'   BibOptions(check.entries = FALSE)
#'   ReadCrossRef(query = 'rj carroll measurement error', limit = 2, sort = "relevance",
#'     min.relevance = 80)
#'
#'   ReadCrossRef(query = 'carroll journal of the american statistical association',
#'     year = 2012, limit = 2)
#' }
ReadCrossRef <- function(query, limit = 5, sort = "relevance", year = NULL,
                         min.relevance = 80, temp.file = tempfile(fileext = ".bib"),
                         delete.file = TRUE, verbose = FALSE){
  if (.is_not_nonempty_text(query))
    stop(gettextf("specify a valid %s", sQuote("query")))
  bad <- 0

  ## file.create(temp.file)
  ## if query is valid doi, skip search and get BibTeX entry right away
  if (!is.na(.doi <- SearchDOIText(query))){
    num.res <- 1
    bad <- GetCrossRefBibTeX(paste0("http://dx.doi.org/", .doi), temp.file)
  }else{
    results <- try(getForm("http://search.crossref.org/dois", q=query, year=year,
                           sort=sort, rows=limit))
    if (inherits(results, "try-error"))
      stop(gettextf("RCurl failed to GET results from CrossRef: %s", geterrmessage()))


    fromj <- RJSONIO::fromJSON(results)
    num.res <- min(limit, length(fromj))
    if(num.res == 0L){
      message(gettextf("query %s returned no matches", dQuote(query)))
      return()
    }

    if (num.res > 0L){
      file.create(temp.file)
      if (delete.file)
        on.exit(unlink(temp.file, force = TRUE))

     # entries <- vector('character', num.res)
      relevancies <- numeric(num.res)
      for(i in 1:num.res){
          if (fromj[[i]]$normalizedScore >= min.relevance){
              bad <- bad + GetCrossRefBibTeX(fromj[[i]]$doi, temp.file)
              if (verbose)
                  message(gettextf("including the following entry with relevancy score %s:\n%s",
                               fromj[[i]]$fullCitation, fromj[[i]]$normalizedScore))
          }
      }
    }
  }  # end else for not DOI query case
  if (bad == num.res){
    message(gettextf("no results with relavency score greater than %s successfully retrieved",
                       sQuote("min.relevance")))
    return()
  }

  bib.res <- try(ReadBib(file=temp.file, .Encoding='UTF-8'), TRUE)
  if (inherits(bib.res, "try-error"))
      stop(gettextf("failed to parse the returned BibTeX results; if \'delete.file\' %s%s",
                     "is FALSE, you can try viewing and editing the file: ", temp.file))

  return(bib.res)
}

#' @keywords internal
GetCrossRefBibTeX <- function(doi, tmp.file){
  temp <- try(getURLContent(url=doi,
               .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"),
                     followLocation=TRUE)), TRUE)
  if(is.raw(temp))
    temp <- rawToChar(temp)
  if (inherits(temp, "try-error") || temp[1] == "<h1>Internal Server Error</h1>" ||
      !grepl("^[[:space:]]*@", temp, useBytes = TRUE)){
      ## last one for occasional non-bibtex returned by CrossRef
    message(gettextf("server error for doi %s, you may want to try again.", dQuote(doi)))
    return(1L)
  }
  temp <- gsub("&amp;", "&", temp, useBytes = TRUE)
  ## Crossref uses data type for some entries
  temp <- sub("^@[Dd]ata", "@online", temp, useBytes = TRUE)
  write(temp, file = tmp.file, append=TRUE)
  return(0L)
}
