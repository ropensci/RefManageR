# Search for a date in a BibEntry object
#
# Find a date in a date field of a BibEntry object
#' @keywords internal
#' @importFrom lubridate is.interval year month int_end int_start
MatchDate <- function(x, pattern, match.date = .BibOptions$match.date){
  if (is.null(x))
    return(FALSE)

  if (identical(match.date, "year.only")){
    if (is.interval(x)  && is.interval(pattern)){
      return(year(int_start(x)) >= year(int_start(pattern)) && year(int_end(x)) <= year(int_end(pattern)))
    }else if (is.interval(x)){
      return(year(int_start(x)) >= year(pattern) && year(int_end(x)) <= year(pattern))
    }else if (is.interval(pattern)){
      return(year(x) >= year(int_start(pattern)) && year(x) <= year(int_end(pattern)))
    }else{
      return(year(x) == year(pattern))
    }
  }else{
    if (is.interval(pattern)){
      return(x %within% pattern)
    }else{
      if (is.interval(x)){
        return(pattern %within% x)
      }else{
        return(x == pattern)
      }
    }
  }
}

# Search for a name in a BibEntry object
#
# Find name in a name field of a BibEntry object
#
#' @keywords internal
MatchName <- function(nom, pattern, match.author=.BibOptions$match.author, ign.case = .BibOptions$ignore.case,
                      regx = .BibOptions$use.regex){
  if (is.null(nom))
    return(FALSE)
  if (identical(match.author, "exact")){
    nom <- as.character(nom)
  }else if (identical(match.author, 'family.with.initials')){
    nom <- sapply(nom, function(x) paste0(paste0(substring(x$given, 1L, 1L), collapse = ''),
                                          paste0(x$family, collapse = '')))
  }else{
    nom <- sapply(nom$family, paste0, collapse = '')
  }

  if (!regx && ign.case){
    return(all(pattern %in% tolower(nom)))
  }else{
      return(all(sapply(pattern, function(pat) any(grepl(pat, x = nom, fixed = !regx,
                                                         ignore.case = ign.case,
                                                         useBytes = TRUE)))))
  }
}

#' Search BibEntry objects by field
#'
#' Allows for searching and indexing a BibEntry object by fields, including names and dates.  The extraction operator and
#' the SearchBib function simplying provide different interfaces to the same search functionality.
#'
#' @param x an object of class BibEntry
#' @param i A named list or character vector of search terms with names corresponding to the field to search for the
#' search term.  Alternatively, a vector of entry key values or numeric or logical indices specifying which bibentries to exctract.
#' @param j A named list or character vector, as \code{i}.  Entries matching the search specified by i \emph{OR} matching
#' the query specified by \code{j} will be return
#' @param ... arguments in the form \code{bib.field = search.term}, or as \code{j} list\emph{s} or character vector\emph{s}
#' for additional searches.  For \code{SearchBib}, can alternatively have same form as \code{i}.
#' @param drop logical, should attributes besides class be dropped from result?
#' @return an object of class BibEntry (the results of the search/indexing), \emph{or} if
#' \code{BibOptions()$return.ind=TRUE}, the indices in \code{x} that match the search terms.
#' @note The arguments to the SearchBib function that control certain search features can also be changed for the extraction
#' operator by changing the corresponding option in the .BibOptions object; see \code{\link{BibOptions}}.
#' @method [ BibEntry
#' @aliases [.BibEntry
#' @export
#' @importFrom lubridate int_start int_end year month is.interval %within%
#' @keywords database manip list
#' @family operators
#' @rdname SearchBib
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#'
#' ## author search, default is to use family names only for matching
#' bib[author = "aristotle"]
#'
#' ## Aristotle references before 1925
#' bib[author="aristotle", date = "/1925"]
#'
#' ## Aristotle references before 1925 *OR* references with editor Westfahl
#' bib[list(author="aristotle", date = "/1925"),list(editor = "westfahl")]
#'
#' ## Change some searching and printing options and search for author
#' old.opts <- BibOptions(bib.style = "authoryear", match.author = "exact",
#'   max.names = 99, first.inits = FALSE)
#' bib[author="Mart\u00edn, Jacinto and S\u00e1nchez, Alberto"]
#' BibOptions(old.opts)  ## reset options
#'
#' ## Some works of Raymond J. Carroll's
#' file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#' bib <- ReadBib(file.name)
#' length(bib)
#'
#' ## index by key
#' bib[c("chen2013using", "carroll1978distributions")]
#'
#' ## Papers with someone with family name Wang
#' length(SearchBib(bib, author='Wang', .opts = list(match.author = "family")))
#'
#' ## Papers with Wang, N.
#' length(SearchBib(bib, author='Wang, N.', .opts = list(match.author = "family.with.initials")))
#'
#' ## tech reports with Ruppert
#' length(bib[author='ruppert',bibtype="report"])
#'
#' ##Carroll and Ruppert tech reports at UNC
#' length(bib[author='ruppert',bibtype="report",institution="north carolina"])
#'
#' ## Carroll and Ruppert papers since leaving UNC
#' length(SearchBib(bib, author='ruppert', date="1987-07/",
#'    .opts = list(match.date = "exact")))
#'
#' ## Carroll and Ruppert papers NOT in the 1990's
#' \dontrun{
#' length(SearchBib(bib, author='ruppert', date = "!1990/1999"))
#' identical(SearchBib(bib, author='ruppert', date = "!1990/1999"),
#'   SearchBib(bib, author='ruppert', year = "!1990/1999"))
#' table(unlist(SearchBib(bib, author='ruppert', date="!1990/1999")$year))
#'
#' ## Carroll + Ruppert + Simpson
#' length(bib[author="Carroll, R. J. and Simpson, D. G. and Ruppert, D."])
#'
#' ## Carroll + Ruppert OR Carroll + Simpson
#' length(bib[author=c("Carroll, R. J. and Ruppert, D.", "Carroll, R. J. and Simpson, D. G.")])
#'
#' ## Carroll + Ruppert tech reports at UNC "OR" Carroll and Ruppert JASA papers
#' length(bib[list(author='ruppert',bibtype="report",institution="north carolina"),
#'   list(author="ruppert",journal="journal of the american statistical association")])
#' }
`[.BibEntry` <- function(x, i, j, ..., drop = FALSE){

  if (!length(x) || (missing(i) && missing(...)))
    return(x)
  ind <- NULL
  if (missing(i)){
    dots <- list(...)
  }else if (is.numeric(i)){  # obvious indices
    ind <- i
  }else if (is.logical(i)){
    ind <- which(i)
  }else if (is.character(i) && missing(j) && missing(...)){
    if (is.null(names(i))){  # assume keys
      ind <- match(i, names(x))
      ind <- ind[!is.na(ind)]
    }else{
      dots <- as.list(i)  # names correspond to fields, value to search terms
    }
  }else if (is.list(i)  && missing(j) && missing(...)){
    dots <- i
  }else if (is.list(i) || is.character(i)){
    kall <- match.call(expand.dots = FALSE)
    if (!missing(j)){
      kall$j <- NULL
    }
    if (!missing(...)){
      kall$`...` <- NULL
    }
    if (is.list(i[[1L]])){
      kall$i <- i[[1L]]
      kall$j <- i[[-1L]]
    }
    ret.ind <- .BibOptions$return.ind
    .BibOptions$return.ind <- TRUE
    tryCatch({
      kall$x <- force(x)  # bquote(.(x))  # needed so that repeated calls don't cause testthat and check errors
      ind <- suppressMessages(eval(kall))
      if (!missing(j)){
        if (is.list(j[[1L]])){  # original call had at least two lists in ...
          kall$i <- j[[1L]]
          kall$j <- j[[-1L]]
        }else{
          kall$i <- j
          if (!missing(...)){
            kall$j <- list(...)
          }
        }
        ind <- unique(c(ind, suppressMessages(eval(kall))))
      }else if (!missing(...)){
        kall$i <- list(...)
        ind <- unique(c(ind, suppressMessages(eval(kall))))
      }
    }, error = function(e){
      .BibOptions$return.ind <- ret.ind
      stop(e)
    })
    .BibOptions$return.ind <- ret.ind
  }else{
    stop("Invalid index.")
  }
  if (exists("dots", inherits = FALSE)){
    add <- function(x) suppressMessages(Reduce("|", x))
    y <- .BibEntry_expand_crossrefs(x)
    fields <- names(dots)
    ind <- seq_along(x)
    for (i in seq_along(dots)){
      ind <- ind[add(lapply(dots[[i]], function(trm, bib, fld){
          len <- nchar(trm)
          if (len > 2L && substr(trm, 1L, 1L) == "!"){
            trm <- substr(trm, 2L, len)
            !FindBibEntry(bib, trm, fld)
          }else{
            FindBibEntry(bib, trm, fld)
          }
        }, bib = y[[ind]], fld = fields[i]))]
      if (!length(ind))
        break
    }
  }

  if (.BibOptions$return.ind)
    return(ind)
  if (!length(ind)){
    message("No results.")
    return(invisible(list()))
  }
  y <- .BibEntry_expand_crossrefs(unclass(x[[ind]]), unclass(x[[-ind]]))
  if (!drop)
    attributes(y) <- attributes(x)[bibentry_list_attribute_names]
  class(y) <- c('BibEntry', 'bibentry')
  return(y)
}

#' Find a search term in the specified field of a BibEntry object
#'
#' Workhorse function for SearchBib
#'
#' @keywords internal
FindBibEntry <- function(bib, term, field){
  usereg <- .BibOptions$use.regex
  ignorec <- .BibOptions$ignore.case
  if (d.yr <- field %in% c('date', 'year')){
    vals <- do.call('$', list(x = bib, name = 'dateobj'))
  }else{
    vals <- do.call('$', list(x = bib, name = field))
  }
  if (length(bib) == 1)
    vals <- list(vals)
  if (!length(unlist(vals))){
    res <- logical(length(bib))
  }else if (field %in% .BibEntryNameList){
    match.aut <- .BibOptions$match.author
    if (TRUE){  # !usereg
      term <- ArrangeAuthors(term)
      if (match.aut == 'exact'){
        term <- as.character(term)
      }else if (match.aut == 'family.with.initials'){
        term <- sapply(term, function(x) paste0(paste0(substring(x$given, 1L, 1L), collapse = ''),
                                                paste0(x$family, collapse = '')))
      }else{
        term <- sapply(term$family, paste0, collapse = ' ')
      }
    }
    if (ignorec)
      term <- tolower(term)
    res <- sapply(vals, MatchName, pattern = term, match.author = match.aut, regx = usereg, ign.case = ignorec)
  }else if (field %in% .BibEntryDateField){
    if (field == 'month'){
      res <- sapply(vals, pmatch, table = term, nomatch = FALSE)
    }else{
      if (d.yr){
        match.dat <- ifelse(field == 'year', 'year.only', .BibOptions$match.date)
      }else{  # eventdate, origdate, urldate
        vals <- lapply(vals, function(x){
          res <- try(ProcessDate(x), TRUE)
          if (inherits(res, 'try-error'))
            return()
          res
        })
        match.dat <- .BibOptions$match.date
      }
      term <- try(ProcessDate(term, NULL, TRUE))
      if (is.null(term) || inherits(term, 'try-error')){
        res <- logical(length(bib))
      }else{
        res <- sapply(vals, MatchDate, pattern = term, match.date = match.dat)
      }
    }
  }else if (field == "dateobj"){
    if (.BibOptions$match.date == 'year.only'){
      vals <- lapply(vals, year)
      term <- year(term)
    }
    res <- sapply(vals, `==`, term)
    res[sapply(res, length)==0] <- FALSE
  }else{
    res <- logical(length(bib))
    not.nulls <- which(!sapply(vals, is.null))
    vals <- gsub('\\n[[:space:]]*', ' ', unlist(vals[not.nulls]), useBytes = TRUE)
    vals <- unlist(strsplit(cleanupLatex(vals), '\n') )

    if (!usereg && ignorec){
      res[not.nulls[grepl(tolower(term), tolower(vals), fixed = TRUE, useBytes = TRUE)]] <- TRUE
    }else{
        res[not.nulls[grepl(term, vals, fixed = !.BibOptions$use.regex,
                            ignore.case = .BibOptions$ignore.case, useBytes = TRUE)]] <- TRUE
    }
  }
  res
}
