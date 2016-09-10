# Mathew W. McLean
# December 1, 2013
# Interact with PubMed

# http://www.ncbi.nlm.nih.gov/books/NBK25500/
# http://www.bioinformatics.org/texmed/
# http://www.poirrier.be/~jean-etienne/software/pyp2b/
# http://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
# http://ropensci.github.io/rebi/

#' Search NCBI's Entrez for citation information
#'
#' This function takes a query and searches an Entrez database for
#' references using NCBI's E-Utilities, returning the results in a BibEntry object.
#' @param query string; search term.
#' @param database string; the Entrez database to search.
#' @param ... additional parameters to use for the search.
#' See the \emph{Details}.
#' @return an object of class BibEntry.
#' @export
#' @details Optional additional parameters to pass to the server include
#' \itemize{
#' \item \code{retstart} - index of the first retrieved ID that should be included in the results.
#' \item \code{retmax} - maximum number of IDs the server will return.
#' \item \code{field} - limits the query to search only the specified field (e.g. \dQuote{title}).
#' \item \code{datetype} - type of date to use when limiting search by dates. E.g. \dQuote{mdat} for
#' modification date or \dQuote{pdat} for publication date.
#' \item \code{reldate} - integer; only items that have (\code{datetype}) date values within \code{reldate} \emph{days}
#' of the current date will be returned.
#' \item \code{mindate}, \code{maxdate} - date ranges to restrict search results.  Possible formats are
#' \dQuote{YYYY}, \dQuote{YYYY/MM}, and \dQuote{YYYY/MM/DD}.
#' }
#' @note The returned entries will have type either \sQuote{Article} or \sQuote{Misc} depending on whether journal information was retrieved.
#' See the Entrez documentation listed in the \emph{References}.
#'
#' The language of the entry will be returned in the field \dQuote{language} and the abstract will be returned in the field \dQuote{abstract}, if they are available.
#' @references \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}
#' @family pubmed
#' @examples
#' if (interactive() && url.exists("http://eutils.ncbi.nlm.nih.gov/"))
#'   ReadPubMed(query = "raymond carroll measurement error", retmax = 5, mindate = 1990)
ReadPubMed <- function(query, database = 'PubMed', ...){
  .params <- list(...)
  bad.ind <- which(!names(.params) %in% c('usehistory', 'WebEnv', 'query_key',
                                          'retstart', 'retmax', 'field',
                                          'datetype', 'reldate', 'mindate', 'maxdate'))
  .parms <- .params
  if (length(bad.ind)){
    warning("Invalid .params specified and will be ignored")
    .parms <- .parms[-bad.ind]
  }
  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  stopifnot(!missing(query))

  .parms$term <- query
  .parms$db <- database
  .parms$usehistory = 'y'
  results <- try(getForm(base.url, .params = .parms))
  if (inherits(results, 'try-error'))
    return(NA)

  tdoc <- xmlParse(results)

  webenv <- unlist(xpathApply(tdoc, '/eSearchResult/WebEnv', xmlValue))
  query.key <- unlist(xpathApply(tdoc, '/eSearchResult/QueryKey', xmlValue))
  ids <- unlist(xpathApply(tdoc, '/eSearchResult/IdList/Id', xmlValue))
  if (!length(ids)){
    message('No Results.')
    return()
  }

  res <- GetPubMedByID(id = ids, db = database, WebEnv = webenv, query_key = query.key)

  return(res)
}

#' Retrieve citation information from NCBI's Entrez for a set of PubMed IDs
#'
#' Uses NCBI's E-Utilities to retrieve bibliographic information given a vector of PubMed ID's and returns
#' the results as a BibEntry object.
#' @param id character vector; PubMed ID's for searching NCBI's Entrez.
#' @param db string; Entrez database to search.
#' @param ... additional parameters to use for the search.
#' See the Entrez documentation listed in the \emph{References}.
#' @return a BibEntry object.
#' @note Returned entries will have \code{bibtype} \dQuote{Article} or \dQuote{Book},
#' unless a collection title is present -- in which case the \code{bibtype} will be
#' \dQuote{InBook} -- or there is no journal information returned for an article -- in
#' which case the \code{bibtype} will be \dQuote{Misc}.
#' @importFrom RCurl getForm
#' @importFrom XML xmlParse getNodeSet
#' @keywords database
#' @export
#' @references \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @family pubmed
#' @examples
#' if (interactive() && url.exists("http://eutils.ncbi.nlm.nih.gov/"))
#'   GetPubMedByID(c("11209037", "21245076"))
GetPubMedByID <- function(id, db = 'pubmed', ...){

  parms <- list(db = db, ...)
  if (!missing(id))
    parms$id <- paste0(id, collapse=',')

  parms$retmode <- 'xml'
  parms$rettype <- 'medline'

  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  temp <- getForm(base.url, .params = parms)
  tdoc <- xmlParse(temp)

  # Note: directly using xpathApply on tdoc won't work if some results are missing certain fields
  results <- getNodeSet(tdoc, '//PubmedArticleSet/PubmedArticle')
  results.book <- getNodeSet(tdoc, '//PubmedArticleSet/PubmedBookArticle')

  ## if(!length(results) && !length(results.book)){
  ##   message('No results.')
  ##   return()
  ## }

  results <- c(lapply(results, ProcessPubMedResult),
               lapply(results.book, ProcessPubMedBookResult))

  res <- if (length(results))  # else NULL
            MakeCitationList(results)
  if (length(fails <- setdiff(id, unlist(res$eprint))))
     message(paste0("Unable to fetch entries for id's: ",
                    paste0(fails, collapse = ", ")))
  
  res <- MakeKeysUnique(res)

  return(res)
}

#' Retrieve related articles from PubMed using PubMed ID's
#'
#' Searches PubMed for articles related to a set of PubMed ID's using NCBI's E-Utilities.
#' @param id either a character vector of PubMed ID's or a BibEntry object, which is expected to have at least some
#' entries with \code{eprinttype = "pubmed"} and eprint field specifying a PubMed ID.
#' @param database string; the Entrez database to search
#' @param batch.mode logical; if \code{TRUE}, the PubMed IDs in \code{id} are combined by Entrez when searching for linked
#' IDs so that only one set of linked IDs is returned.  If \code{FALSE}, a set of linked IDs is obtained for each ID
#' in \code{id}.
#' will be returned
#' @param max.results numeric vector; the maximum number of results to return if \code{batch.mode}
#' \code{TRUE}; or if \code{batch.mode} is \code{FALSE}, this should have the same length
#' as \code{id} with each element giving the maximum number of results to return for the
#' corresponding ID.
#' @param return.sim.scores logical; Entrez returns a similarity score with each returned citation giving a
#' measure of how similar the returned entry is to the ones specified by the query.  If \code{TRUE} these scores are added
#' to the returned BibEntry object in a field called \sQuote{score}.
#' @param return.related.ids logical; should the original PubMed ID(s) that a returned entry is related to be stored in a
#' field called \sQuote{PMIDrelated}.
#' @return an object of class BibEntry.
#' @importFrom RCurl getForm
#' @importFrom XML xmlDoc xpathApply getNodeSet xmlParse
#' @references \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @family pubmed
#' @keywords database
#' @export
#' @examples
#' if (interactive() && url.exists("http://eutils.ncbi.nlm.nih.gov/")){
#'   file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#'   bib <- ReadBib(file.name)
#'   bib <- LookupPubMedID(bib[[101:102]])
#'   toBiblatex(GetPubMedRelated(bib, batch.mode = TRUE, max.results = 2,
#'   return.sim.scores = TRUE, return.related.ids = TRUE))
#'   GetPubMedRelated(bib, batch.mode = FALSE, max.results = c(2, 2))
#' }
GetPubMedRelated <- function(id, database = 'pubmed', batch.mode = TRUE, max.results = 10,
                             return.sim.scores = FALSE, return.related.ids = FALSE){
  stopifnot(!missing(id))

  if (inherits(id, 'BibEntry')){
    oldind <- BibOptions(return.ind = TRUE)
    ind <- suppressMessages(id[eprinttype = "pubmed"])
    BibOptions(oldind)
    if (!length(ind)){
      message('BibEntry object provided contains no PubMed Ids.')
      return(invisible(NULL))
    }
    id <- unlist(id$eprint[ind])
  }

  if (batch.mode){
    id <- paste0(id, collapse=',')
    parms <- list(cmd = 'neighbor_score', dbfrom = 'pubmed', db = database, id = id)
  }else{
    parms <- as.list(id)
    names(parms) <- rep("id", length(id))
    parms <- c(parms, cmd = "neighbor_score", dbfrom = "pubmed", db = database)
  }
  id.len <- length(id)

  base.url <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi'

  results <- try(getForm(base.url, .params = parms))
  if (inherits(results, 'try-error'))
    return(NA)

  tdoc <- xmlParse(results)
  max.results <- rep(max.results, l = id.len)
  res <- vector("list", id.len)
  ch <- getNodeSet(tdoc, '/eLinkResult/LinkSet')
  for (i in seq_len(id.len)){
    temp <- xmlDoc(ch[[i]])
    if (return.related.ids)
      related <- paste0(unlist(xpathApply(temp, '/LinkSet/IdList/Id',
                                          xmlValue)), collapse = ',')
    temp <- xmlDoc(temp["//LinkSetDb"][[1L]])

    ids <- unlist(xpathApply(temp, '/LinkSetDb/Link/Id', xmlValue))
    if (is.null(id)){
      message('No results.')
      return()
    }

    ind <- seq_len(min(max.results[i], length(ids)))
    if (return.sim.scores)
      scores <- unlist(xpathApply(temp, '/LinkSetDb/Link/Score', xmlValue))[ind]

    tres <- GetPubMedByID(id = ids[ind], db = database)
    if (return.sim.scores)
      tres$score <- scores
    if (return.related.ids)
      tres$PMIDrelated <- rep(related, l = length(ind))
    res[[i]] <- tres
  }
  res <- c(unlist(res, recursive = FALSE))
  class(res) <- c("BibEntry", "bibentry")
  res <- MakeKeysUnique(res)
  return(res)
}

#' @keywords internal
#' @importFrom XML xpathApply xmlValue free xmlGetAttr
ProcessPubMedResult <- function(article){
  if (!length(article))
     return()
  tdoc <- xmlDoc(article)
  res <- list()

  title <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/ArticleTitle',
                                 xmlValue))
  res$title <- gsub('\\.$', '', title, useBytes = TRUE)
  last.names <- unlist(xpathApply(tdoc,
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/LastName',
                                  xmlValue))
  first.names <- unlist(xpathApply(tdoc,
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/ForeName',
                         xmlValue))
  res$author <- as.person(paste(first.names, last.names))
  if (!length(res$author)){
      last.names <- unlist(xpathApply(tdoc,
                '//PubmedArticle/MedlineCitation/Article/AuthorList/Author/CollectiveName',
                                  xmlValue))
      if (length(last.names))
          res$author <- person(last.names)
  }
  complete.AuthorList <- unlist(xpathApply(tdoc,
                    "//PubmedArticle/MedlineCitation/Article/AuthorList",
                                    xmlGetAttr, name = "CompleteYN", default = ""))

  res$year <- unlist(xpathApply(tdoc,
        '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Year',
                                 xmlValue))
  res$month <- unlist(xpathApply(tdoc,
        '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/PubDate/Month',
                                 xmlValue))

    if (is.null(res$year))  # search extra hard for year
        res$year <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/ArticleDate/Year',
                                 xmlValue))
    if (is.null(res$year))
      res$year <- unlist(xpathApply(tdoc, '//PubmedArticle/PubmedData/History/PubMedPubDate/Year',
                                 xmlValue))[1]

  res$journal <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/Title',
                              xmlValue))

  res$volume <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Volume',
                              xmlValue))

  res$number <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Journal/JournalIssue/Issue',
                              xmlValue))

  res$pages <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/Article/Pagination/MedlinePgn',
                            xmlValue))
  res$eprint <- unlist(xpathApply(tdoc, '//PubmedArticle/MedlineCitation/PMID',
                          xmlValue))
  doi <- unlist(xpathApply(tdoc, '//PubmedArticle/PubmedData/ArticleIdList/ArticleId',
                              xmlValue))
  res$doi <- if (length(doi > 1)){
    id.types <- unlist(xpathApply(tdoc,
                    "//PubmedArticle/PubmedData/ArticleIdList/ArticleId",
                                    xmlGetAttr, name = "IdType", default = ""))
    if (length(doi.pos <- grep("doi", id.types, ignore.case = TRUE, useBytes = TRUE)))
       doi[doi.pos[1L]]
    else{
      doi <- sapply(doi, SearchDOIText)
      if (any(doi.pos <- !is.na(doi)))
        doi[doi.pos[1L]]
      else
        NULL
    }
  }else
    doi
  # res$doi <- grep('/', doi, value = TRUE)

  res$language <- unlist(xpathApply(tdoc,
                           "//PubmedArticle/MedlineCitation/Article/Language", xmlValue))
  res$issn <- unlist(xpathApply(tdoc,
                           "//PubmedArticle/MedlineCitation/Article/Journal/ISSN",
                                xmlValue))

  res$abstract <- unlist(xpathApply(tdoc,
                    "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText",
                                    xmlValue))
  if (length(res$abstract) > 1){  # some abstracts are separated into sections: methods, conclusion, etc.
      abstract.labs <- unlist(xpathApply(tdoc,
                    "//PubmedArticle/MedlineCitation/Article/Abstract/AbstractText",
                                    xmlGetAttr, name = "Label", default = ""))
      res$abstract <- if (!any(sapply(abstract.labs, .is_not_nonempty_text)))
                           paste0(paste(abstract.labs, res$abstract, sep = ": "), collapse = "\n")
                      else paste0(res$abstract, collapse = "\n")
  }

  if (!length(complete.AuthorList) || complete.AuthorList == "N")
    warning(paste0("Incomplete list of authors returned by PubMed for ID: ",
                   res$eprint), call. = FALSE)

  free(tdoc)
  res$eprinttype <- 'pubmed'

  attr(res, 'entry') <- if (!is.null(res$journal) && !is.null(res$author))
                          'article'
                        else
                          'misc'

  attr(res, 'key') <- CreateBibKey(res$title, res$author, res$year)
  MakeBibEntry(res, FALSE)
}

ProcessPubMedBookResult <- function(article){
  if (!length(article))
      return()
  tdoc <- xmlDoc(article)
  res <- list()

  title <- unlist(xpathApply(tdoc, '//PubmedBookArticle/BookDocument/Book/BookTitle',
                                 xmlValue))
  res$title <- gsub('\\.$', '', title, useBytes = TRUE)
  last.names <- unlist(xpathApply(tdoc,
                '//PubmedBookArticle/BookDocument/Book/AuthorList/Author/LastName',
                                  xmlValue))
  first.names <- unlist(xpathApply(tdoc,
                '//PubmedBookArticle/BookDocument/Book/AuthorList/Author/ForeName',
                         xmlValue))
  res$author <- as.person(paste(first.names, last.names))

  res$year <- unlist(xpathApply(tdoc,
                       '//PubmedBookArticle/BookDocument/Book/PubDate/Year', xmlValue))
    ## if (is.null(res$year))  # search extra hard for year
    ##     res$year <- unlist(xpathApply(tdoc,
    ##           '//PubmedBookArticle/MedlineCitation/Article/ArticleDate/Year',
    ##                              xmlValue))
  res$month <- unlist(xpathApply(tdoc,
                       '//PubmedBookArticle/BookDocument/Book/PubDate/Month', xmlValue))
  if (is.null(res$year))
    res$year <- unlist(xpathApply(tdoc,
                   '//PubmedBookArticle/PubmedBookData/History/PubMedPubDate/Year',
                               xmlValue))[1]

  res$booktitle <- unlist(xpathApply(tdoc,
                            '//PubmedBookArticle/BookDocument/Book/CollectionTitle',
                              xmlValue))

  res$publisher <- unlist(xpathApply(tdoc,
          '//PubmedBookArticle/BookDocument/Book/Publisher/PublisherName',
                              xmlValue))

  res$location <- unlist(xpathApply(tdoc,
                    '//PubmedBookArticle/BookDocument/Book/Publisher/PublisherLocation',
                              xmlValue))
  res$eprint <- unlist(xpathApply(tdoc, '//PubmedBookArticle/BookDocument/PMID',
                          xmlValue))
  doc.ids <- unlist(xpathApply(tdoc,
                   '//PubmedBookArticle/PubmedBookData/ArticleIdList/ArticleId',
                              xmlValue))
  id.types <- unlist(xpathApply(tdoc,
                    "//PubmedBookArticle/PubmedBookData/ArticleIdList/ArticleId",
                                    xmlGetAttr, name = "IdType", default = ""))
  ## res$eprint <- if(length(pmid.pos <- grep("pubmed", id.types, ignore.case = TRUE)))
  ##                  doc.ids[pmid.pos[1L]]
  res$doi <- if (length(doi.pos <- grep("doi", id.types, ignore.case = TRUE, useBytes = TRUE)))
               doc.ids[doi.pos[1L]]
             else{
               doc.ids <- sapply(doc.ids, SearchDOIText)
               if (any(doi.pos <- !is.na(doc.ids)))
                 doc.ids[doi.pos[1L]]
               else
                 NULL
             }

  # res$doi <- grep('/', doi, value = TRUE)

  res$language <- unlist(xpathApply(tdoc,
                           "//PubmedBookArticle/BookDocument/Language", xmlValue))
  res$abstract <- unlist(xpathApply(tdoc,
                    "//PubmedBookArticle/BookDocument/Abstract/AbstractText",
                                    xmlValue))
  if (length(res$abstract) > 1L){  # some abstracts are separated into sections: methods, conclusion, etc.
      abstract.labs <- unlist(xpathApply(tdoc,
                    "//PubmedBookArticle/BookDocument/Abstract/AbstractText",
                                    xmlGetAttr, name = "Label", default = ""))
      res$abstract <- if (!any(sapply(abstract.labs, .is_not_nonempty_text)))
                           paste0(paste(abstract.labs, res$abstract, sep = ": "), collapse = "\n")
                      else paste0(res$abstract, collapse = "\n")
  }

  free(tdoc)
  res$eprinttype <- 'pubmed'

  attr(res, 'entry') <- if (!is.null(res$booktitle))
                          'InBook'
                        else
                          'Book'

  attr(res, 'key') <- CreateBibKey(res$title, res$author, res$year)

  return(MakeBibEntry(res, FALSE))
}

#' Retrieve PubMed ID's for a BibEntry object
#'
#' Uses the NCBI E-utilities to to search for PubMed ID's for citations stored in a BibEntry object.
#' @param bib a bibentry object
#' @param index indices specifying which entries of \code{bib} will be searched for.  If \code{missing}, all entries
#' are searched for.
#' @return a BibEntry object - \code{bib} with additional eprinttype and eprint fields when the search is successful
#' for an entry.
#' @details For each entry a citation string is created using the fields journaltitle/journal, date/year,
#'   volume, pages, and author; and these strings are then used to search the NCBI database for PubMed ID's.
#'
#'   If an ID is found for an entry, the entry is updated so that the eprinttype field is assigned the value
#'   \dQuote{pubmed} and the eprint field is assigned the ID.
#' @family pubmed
#' @importFrom RCurl getURL
#' @keywords database
#' @export
#' @examples
#' if (interactive() && url.exists("http://eutils.ncbi.nlm.nih.gov/")){
#'   file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#'   bib <- ReadBib(file.name)
#'   LookupPubMedID(bib[[101:102]])
#' }
LookupPubMedID <- function(bib, index){
  stopifnot(inherits(bib, 'BibEntry'))
  if (missing(index))
    index <- seq_along(bib)

  base.url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi?db=pubmed&retmode=xml&bdata="
  cit.strings <- sapply(bib[index], MakeCitationString)

  .url <- paste0(base.url,
                paste0(cit.strings, collapse = '%0D'))
  results <- getURL(.url)

  m <- gregexpr('KeY\\|(NOT_FOUND|[0-9]+)', results, useBytes = TRUE)
  res <- unlist(regmatches(results, m))
  res <- sub('KeY\\|', '', res, useBytes = TRUE)
  ind <- grep('[0-9]', res, useBytes = TRUE)
  if (length(ind)){
    message(paste0('Success for entries: ', paste0(index[ind], collapse=', ')))
    bib$eprint[index[ind]] <- res[ind]
    bib$eprinttype[index[ind]] <- "pubmed"
  }else{
    message("No PubMed ID\'s found")
  }

  return(bib)
}

#' @keywords internal
#' @importFrom lubridate year
#' @importFrom RCurl curlEscape
MakeCitationString <- function(bib.entry){
  first.page <- sub('-[0-9]+', '', bib.entry$pages, useBytes = TRUE)
  res <- paste(bib.entry$journal, year(bib.entry$dateobj), bib.entry$volume, first.page,
        paste0(as.character(bib.entry$author), collapse = ' '), 'KeY', sep = '|')
  res <- curlEscape(res)
  res <- gsub('%7C', '|', res, useBytes = TRUE)
  res <- gsub('%26', 'and', res, useBytes = TRUE)
  res <- paste0(res, '|')
  return(gsub('%20', '+', res, useBytes = TRUE))
}
