#' Search CrossRef for citations.
#'
#' Provides an interface to the CrossRef API, searching for citations given a string query.
#' Results are written to a
#' bib file, read back into \code{R} using \code{\link{WriteBib}}, and returned as a BibEntry object.
#'
#' @param query string; search term
#' @param filter named list of possible filters; see \code{Details} and \code{References};
#' ignored if \code{use.old.api = TRUE}
#' @param limit numeric; maximum number of entries to return
#' @param offset numeric; CrossRef will not return the
#' first \code{offset} results (default 0); ignored if \code{use.old.api = TRUE}
#' @param sort string; how specifying how the results from CrossRef should be sorted.  Possible
#' values when \code{use.old.api = FALSE} are \code{"score"} (default; same as \code{"relevance"}),
#' \code{"updated"}, \code{"deposited"}, \code{"indexed"}, or \code{"published"};
#' see the references
#' @param year numeric; if specified, only results from this year will be returned.
#' @param min.relevance numeric; only results with a CrossRef-assigned relevance score at
#' least this high will be returned.
#' @param temp.file string; file name to use for storing Bibtex information returned by CrossRef.
#' @param delete.file boolean; should the bib file be deleted on exit?
#' @param verbose boolean; if \code{TRUE}, additional messages are output regarding the
#' results of the query.
#' @param use.old.api boolean; should the older CrossRef API be used for the search?
#' @return An object of class \code{BibEntry}.
#' @note The entries returned by Crossref are frequently missing fields required by BibTeX, if
#' you want the entries to be returned anyway, set \code{BibOptions()$check.entries} to
#' \code{FALSE} or \code{"warn"}
#'
#' Fields \code{"score"} (the relevancy score) and \code{"license"} will be returned when
#' \code{use.old.api = FALSE}.
#' @details When \code{use.old.api = TRUE}, the query HTTP request only returns DOIs,
#' which are then used to make HTTP requests for the corresponding BibTeX entries from
#' CrossRef; when \code{use.old.api = FALSE}, the query HTTP request is parsed to create
#' the \code{BibEntry} object (i.e. there are less HTTP requests when using the new API).
#'
#' CrossRef assigns a score between 0 and 100 based on how relevant a reference seems
#' to be to your query.  The \emph{old} API documentation warns that while false
#' negatives are unlikely, the search can be prone to false positives.  Hence, setting
#' \code{min.revelance} to a high value may be necessary if \code{use.old.api = TRUE}.
#'
#' Possible values for the \emph{names} in \code{filter} are \code{"has-funder"}, \code{"funder"},
#' \code{"prefix"}, \code{"member"}, \code{"from-index-date"}, \code{"until-index-date"},
#' \code{"from-deposit-date"}, \code{"until-deposit-date"}, \code{"from-update-date"},
#' \code{"until-update-date"}, \code{"from-created-date"}, \code{"until-created-date"},
#' \code{"from-pub-date"}, \code{"until-pub-date"}, \code{"has-license"}, \code{"license.url"},
#' \code{"license.version"}, \code{"license.delay"}, \code{"has-full-text"},
#' \code{"full-text.version"}, \code{"full-text.type"}, \code{"public-references"},
#' \code{"has-references"}, \code{"has-archive"}, \code{"archive"}, \code{"has-orcid"},
#' \code{"orcid"}, \code{"issn"}, \code{"type"}, \code{"directory"}, \code{"doi"},
#' \code{"updates"}, \code{"is-update"}, \code{"has-update-policy"}, \code{"container-title"},
#' \code{"publisher-name"}, \code{"category-name"}, \code{"type-name"}, \code{"award.number"},
#' \code{"award.funder"}, \code{"assertion-group"}, \code{"assertion"}, \code{"affiliation"},
#' \code{"has-affiliation"}, \code{"alternative-id"}, and \code{"article-number"}.  See the first
#' reference for a description of their meanings.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content http_error add_headers
#' @importFrom utils URLdecode
#' @export
#' @keywords database
#' @seealso \code{\link{ReadZotero}}, \code{\link{BibEntry}}, \code{\link{GetDOIs}},
#' package \code{rcrossref} for larger queries and deep paging
#' @family pubmed
#' @references Newer API: \url{https://github.com/CrossRef/rest-api-doc/blob/master/rest_api.md},
#' Older API: \url{http://search.crossref.org/help/api}
#' @examples
#' if (interactive() && !http_error("http://search.crossref.org/")){
#'   BibOptions(check.entries = FALSE)
#'   ## 3 results from the American Statistical Association involving "regression"
#'   ReadCrossRef("regression", filter = list(prefix="10.1198"), limit = 3)
#'
#'   ## Some JRSS-B papers published in 2010 or later, note the quotes for filter
#'   ##   names with hypens
#'   ReadCrossRef(filter = list(issn = "1467-9868", "from-pub-date" = 2010),
#'                limit = 2, min.relevance = 0)
#' 
#'   ReadCrossRef(filter = list(prefix = "10.5555"), limit = 5, min.relevance = 0)
#'
#'   ## old API
#'   ReadCrossRef(query = 'rj carroll measurement error', limit = 2, sort = "relevance",
#'     min.relevance = 80, use.old.api = TRUE)
#'
#'   ReadCrossRef(query = 'carroll journal of the american statistical association',
#'     year = 2012, limit = 2, use.old.api = TRUE)
#' }
ReadCrossRef <- function(query = "", filter = list(), limit = 5, offset = 0,
                         sort = "relevance", year = NULL,
                         min.relevance = 2, temp.file = tempfile(fileext = ".bib"),
                         delete.file = TRUE, verbose = FALSE, use.old.api = FALSE){
  bad <- 0
  
  ## file.create(temp.file)
  ## if query is valid doi, skip search and get BibTeX entry right away

  if (!is.na(.doi <- SearchDOIText(query))){
    num.res <- 1
    bad <- GetCrossRefBibTeX(paste0("http://dx.doi.org/", .doi), temp.file)
  }else{
    if (use.old.api){
      if (.is_not_nonempty_text(query))
          stop(gettextf("specify a valid %s", sQuote("query")))
      headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json')
      results <- try(GET("http://search.labs.crossref.org/dois", q=query, year=year,
                           sort=sort, rows=limit, config = list(add_headers = headers)), TRUE)        
    }else{
      params <- list(rows = limit, sort = sort, offset = offset)
      if (!.is_not_nonempty_text(query))
        params$query <- query  # stop(gettextf("specify a valid %s", sQuote("query")))

      if (length(year))
          suppressWarnings(filter$"from-pub-date" <- filter$"until-pub-date" <- year)

      if (length(filter))
          params$filter <- paste(paste0(names(filter),":",filter), collapse = ",")          
      results <- try(GET("http://api.crossref.org/works", query=params))
    }
    if (inherits(results, "try-error"))
      stop(gettextf("httr failed to GET results from CrossRef: %s", geterrmessage()))

    fromj <- content(results, type = "application/json", encoding = "UTF-8")
    if (!use.old.api)
        fromj <- fromj$message$items
    num.res <- min(limit, length(fromj))
    if(num.res == 0L){
      message(gettextf("Query %s returned no matches", ifelse(!.is_not_nonempty_text(query),
                                                                dQuote(query),
                                                                dQuote(params$filter))))
      return()
    }

    if (num.res > 0L){
        if (!use.old.api && is.na(.doi)){
          res <- lapply(fromj, ParseCrossRef)
          good <- which(sapply(res, function(e){
              good <- e$score >= min.relevance
              if (good && verbose)
                 message(gettextf("including the following entry with relevancy score %s:\n%s",
                                  e$title, e$score[[i]]))
              good
          }))
          res <- res[good]
          if (!length(res)){
              message("no results with relavency score greater than ",
                      gettextf("%s successfully retrieved",
                           sQuote("min.relevance")))
            return()
          }
          class(res) <- c("BibEntry", "bibentry")
          return(res)
      }else{
        file.create(temp.file)
        if (delete.file)
          on.exit(unlink(temp.file, force = TRUE))

        relevancies <- numeric(num.res)
        if (use.old.api){
            score.str <- "normalizedScore"
            doi.str <- "doi"
            entry.str <- "fullCitation"
        }else{
            score.str <- "score"
            doi.str <- "DOI"
            entry.str <- "title"
        }
        for(i in 1:num.res){
          if (fromj[[i]][[score.str]] >= min.relevance){
              bad <- bad + GetCrossRefBibTeX(paste0(if (!use.old.api) "http://dx.doi.org/",
                                                    fromj[[i]][[doi.str]]), temp.file)
              if (verbose)
                  message(gettextf("including the following entry with relevancy score %s:\n%s",
                               fromj[[i]][[entry.str]], fromj[[i]][[score.str]]))
          }
        }
      }  # end else for old API processing
    }
  }  # end else for case when query is not a DOI
  if (bad == num.res){
    message(gettextf("no results with relavency score greater than %s successfully retrieved",
                       sQuote("min.relevance")))
    return()
  }

  bib.res <- try(ReadBib(file=temp.file, .Encoding='UTF-8'), TRUE)
  bib.res$url <- URLdecode(bib.res$url)
  if (inherits(bib.res, "try-error"))
      stop(gettextf("failed to parse the returned BibTeX results; if \'delete.file\' %s%s",
                     "is FALSE, you can try viewing and editing the file: ", temp.file))

  return(bib.res)  
}

#' @keywords internal
GetCrossRefBibTeX <- function(doi, tmp.file){
    ## temp <- try(getURLContent(url=doi,
    ##                 .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"),
    ##                    followLocation=TRUE)), TRUE)
    temp <- try(GET(doi, config = list(followlocation = TRUE),
                      add_headers(Accept = "application/x-bibtex")), TRUE)
    temp <- try(content(temp, as = "text", encoding = "UTF-8"), TRUE)
    ## if(is.raw(temp))
    ##     temp <- rawToChar(temp)
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

#' @keywords internal
ParseCrossRef <- function(e){
    name.fields <- intersect(names(e), .BibEntryNameList)
    for (fld in name.fields)
        e[[fld]] <- ToPersonCR(e[[fld]])
    e <- ProcessDatesCR(e)
    out <- if (length(name.fields))
               e[name.fields]
           else list()
    out <- c(out, title = e$title, subtitle = e$subtitle, date = e$date,
             volume = e$volume, number = e$issue, pages = e$page,
             url = e$URL, doi = e$DOI, issn = e$ISSN[1],
             license = e$license[[1]]$"content-version", score = e$score,
             journaltitle = e$"container-title"[1])

    key <- CreateBibKey(e$title, e$author,
                        sub("^([0-9]{4})[0-9-]*", "\\1", e$date))
    bibtype <- e$type
    bibtype <- if (grepl("journal", bibtype))
                   "Article"
               else if (grepl("book-chapter", bibtype, fixed = TRUE))
                   "InCollection"
               else if (grepl("book", bibtype))
                   "Book"
               else
                   "Misc"
    attr(out, "bibtype") <- bibtype
    attr(out, "key") <- key
    attr(out, "dateobj") <- attr(e, "dateobj")
    out
}

#' @keywords internal
ToPersonCR <- function(x){
    x <- lapply(x, function(y){
                   y$affiliation <- NULL
                   c(y, role = list(NULL), email = list(NULL), comment = list(NULL))
                   })
    # out <- do.call("c", x)
    class(x) <- "person"
    x
}

ProcessDatesCR <- function(e){
    for (s in c("published-print", "issued", "created", "deposited")){
        tdate <- unlist(e[[s]][[1]])
        if (!is.null(tdate))
            break
    }
    if (is.null(tdate))
        return(e)

    daymon <- length(tdate) - 1
    tdate <- paste(tdate, collapse = "-")
    if (daymon == 1)
        tdate <- paste0(tdate, "-01")
    else if (daymon == 0)
        tdate <- paste0(tdate, "-01-01")
    e$date <- tdate
    tdate <- as.POSIXct(tdate)
    attr(tdate, "day.mon") <- daymon
    attr(e, "dateobj") <- tdate
    e
}
