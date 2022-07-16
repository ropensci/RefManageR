# https://www.ncbi.nlm.nih.gov/books/NBK25500/
# https://www.bioinformatics.org/texmed/
# https://web.archive.org/web/20140813080014/http://www.poirrier.be/~jean-etienne/software/pyp2b/
# https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
# https://ropensci.github.io/rebi/

#' Search NCBI's E-Utilities for citation information
#'
#' This function takes a query and searches an Entrez database for
#' references using NCBI's E-Utilities, returning the results in a BibEntry
#' object.
#' @param query string; search term.
#' @param database string; the Entrez database to search.
#' @param ... additional parameters to use for the search.
#' See the \emph{Details}.
#' @return an object of class BibEntry.
#' @export
#' @details Optional additional parameters to pass to the server include
#' \itemize{
#' \item \code{retstart} - index of the first retrieved ID that should be
#' included in the results.
#' \item \code{retmax} - maximum number of IDs the server will
#' return (default 20).
#' \item \code{field} - limits the query to search only the specified
#' field (e.g. \dQuote{title}).
#' \item \code{datetype} - type of date to use when limiting search by
#' dates. E.g. \dQuote{mdat}
#' for modification date or \dQuote{pdat} for publication date.
#' \item \code{reldate} - integer; only items that have (\code{datetype})
#' date values within \code{reldate} \emph{days}
#' of the current date will be returned.
#' \item \code{mindate}, \code{maxdate} - date ranges to restrict search
#' results.  Possible formats are
#' \dQuote{YYYY}, \dQuote{YYYY/MM}, and \dQuote{YYYY/MM/DD}.
#' }
#' @note The returned entries will have type either \sQuote{Article} or
#' \sQuote{Misc} depending on whether journal information was retrieved.
#' See the Entrez documentation listed in the \emph{References}.
#'
#' The language of the entry will be returned in the field \dQuote{language}
#' and the abstract will be returned in the field \dQuote{abstract}, if they
#' are available.
#' @references \url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}
#' @family pubmed
#' @importFrom httr GET http_error stop_for_status
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @examples
#' if (interactive() && !httr::http_error("https://eutils.ncbi.nlm.nih.gov/"))
#'   ReadPubMed(query = "raymond carroll measurement error", retmax = 5, mindate = 1990)
ReadPubMed <- function(query, database = "PubMed", ...){
  .params <- list(...)
  bad.ind <- which(!names(.params) %in% c("usehistory", "WebEnv", "query_key",
                                          "retstart", "retmax", "field",
                                          "datetype", "reldate", "mindate",
                                          "maxdate"))
  .parms <- .params
  if (length(bad.ind)){
    warning("Invalid .params specified and will be ignored")
    .parms <- .parms[-bad.ind]
  }
  base.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

  stopifnot(!missing(query))

  .parms$term <- query
  .parms$db <- database
  .parms$usehistory <- "y"
  ## results <- try(getForm(base.url, .params = .parms))
  results <- GET(base.url, query = .parms)
  stop_for_status(results)

  tdoc <- read_xml(results, encoding = "UTF-8")

  webenv <- xml_text(xml_find_all(tdoc, "//eSearchResult/WebEnv"))
  query.key <- xml_text(xml_find_all(tdoc, "//eSearchResult/QueryKey"))
  ids <- xml_text(xml_find_all(tdoc, "/eSearchResult/IdList/Id"))
  if (!length(ids)){
    message("No Results.")
    return()
  }

  res <- GetPubMedByID(id = ids, db = database, WebEnv = webenv,
                       query_key = query.key)

  return(res)
}

#' Retrieve citation information from NCBI's Entrez for a set of PubMed IDs
#'
#' Uses NCBI's E-Utilities to retrieve bibliographic information given a
#' vector of PubMed ID's and returns the results as a BibEntry object.
#' @param id character vector; PubMed ID's for searching NCBI's Entrez.
#' @param db string; Entrez database to search.
#' @param ... additional parameters to use for the search.
#' See the Entrez documentation listed in the \emph{References}.
#' @return a BibEntry object.
#' @note Returned entries will have \code{bibtype} \dQuote{Article} or \dQuote{Book},
#' unless a collection title is present -- in which case the \code{bibtype} will be
#' \dQuote{InBook} -- or there is no journal information returned for an article -- in
#' which case the \code{bibtype} will be \dQuote{Misc}.
#' @importFrom httr POST
#' @importFrom xml2 xml_text xml_find_all read_xml
#' @keywords database
#' @export
#' @references \url{https://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @family pubmed
#' @examples
#' if (interactive() && !httr::http_error("https://eutils.ncbi.nlm.nih.gov/"))
#'   GetPubMedByID(c("11209037", "21245076"))
GetPubMedByID <- function(id, db = "pubmed", ...){

  parms <- list(db = db, ...)
  if (!missing(id))
    parms$id <- paste0(id, collapse=",")

  parms$retmode <- "xml"
  parms$rettype <- "medline"

  base.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  ## temp <- postForm(base.url, .params = parms)
  ## tdoc <- xmlParse(temp)
  temp <- POST(base.url, query = parms)
  tdoc <- read_xml(temp)

  ## Note: directly using xpathApply on tdoc won't work if some results are
  ##  missing certain fields
  results <- xml_find_all(tdoc, "//PubmedArticleSet/PubmedArticle")
  results.book <- xml_find_all(tdoc, "//PubmedArticleSet/PubmedBookArticle")

  ## if(!length(results) && !length(results.book)){
  ##   message("No results.")
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
#' Searches PubMed for articles related to a set of PubMed ID's using
#' NCBI's E-Utilities.
#' @param id either a character vector of PubMed ID's or a BibEntry object,
#' which is expected to have at least some entries with
#' \code{eprinttype = "pubmed"} and eprint field specifying a PubMed ID.
#' @param database string; the Entrez database to search
#' @param batch.mode logical; if \code{TRUE}, the PubMed IDs in \code{id}
#' are combined by Entrez when searching for linked
#' IDs so that only one set of linked IDs is returned.  If \code{FALSE}, a
#' set of linked IDs is obtained for each ID
#' in \code{id}.
#' will be returned
#' @param max.results numeric vector; the maximum number of results to
#' return if \code{batch.mode} \code{TRUE}; or if \code{batch.mode} is
#' \code{FALSE}, this should have the same length
#' as \code{id} with each element giving the maximum number of results to
#' return for the corresponding ID.
#' @param return.sim.scores logical; Entrez returns a similarity score with
#' each returned citation giving a measure of how similar the returned entry
#' is to the ones specified by the query.  If \code{TRUE} these scores are added
#' to the returned BibEntry object in a field called \sQuote{score}.
#' @param return.related.ids logical; should the original PubMed ID(s) that a
#' returned entry is related to be stored in a field called \sQuote{PMIDrelated}.
#' @return an object of class BibEntry.
#' @importFrom httr GET
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @references \url{https://www.ncbi.nlm.nih.gov/books/NBK25500/}
#' @family pubmed
#' @keywords database
#' @export
#' @examples
#' if (interactive() && !httr::http_error("https://eutils.ncbi.nlm.nih.gov/")){
#'   file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#'   bib <- ReadBib(file.name)
#'   bib <- LookupPubMedID(bib[[101:102]])
#'   toBiblatex(GetPubMedRelated(bib, batch.mode = TRUE, max.results = 2,
#'   return.sim.scores = TRUE, return.related.ids = TRUE))
#'   GetPubMedRelated(bib, batch.mode = FALSE, max.results = c(2, 2))
#' }
GetPubMedRelated <- function(id, database = "pubmed", batch.mode = TRUE,
                             max.results = 10, return.sim.scores = FALSE,
                             return.related.ids = FALSE){
  stopifnot(!missing(id))

  if (inherits(id, "BibEntry")){
    oldind <- BibOptions(return.ind = TRUE)
    ind <- suppressMessages(id[eprinttype = "pubmed"])
    BibOptions(oldind)
    if (!length(ind)){
      message("BibEntry object provided contains no PubMed Ids.")
      return(invisible(NULL))
    }
    id <- unlist(id$eprint[ind])
  }

  if (batch.mode){
    id <- paste0(id, collapse=",")
    parms <- list(cmd = "neighbor_score", dbfrom = "pubmed", db = database,
                  id = id)
  }else{
    parms <- as.list(id)
    names(parms) <- rep("id", length(id))
    parms <- c(parms, cmd = "neighbor_score", dbfrom = "pubmed", db = database)
  }
  id.len <- length(id)

  base.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"

  ## results <- try(getForm(base.url, .params = parms))
  results <- GET(base.url, query = parms)
  if (http_error(results))
      stop(gettextf("NCBI E-Utilities query error [%s]", status_code(results)),
           .call = FALSE)

  tdoc <- read_xml(results)
  max.results <- rep(max.results, l = id.len)
  res <- vector("list", id.len)
  ## ch <- getNodeSet(tdoc, "/eLinkResult/LinkSet")
  ch <- xml_find_all(tdoc, "/eLinkResult/LinkSet")
  for (i in seq_len(id.len)){
    ## temp <- xmlDoc(ch[[i]])
    temp <- ch[i]
    if (return.related.ids)
      related <- paste0(xml_text(xml_find_all(temp, "//LinkSet/IdList/Id")),
                        collapse = ",")
    ## temp <- xmlDoc(temp["//LinkSetDb"][[1L]])

    ids <- xml_text(xml_find_all(temp, ".//LinkSetDb/Link/Id"))
    if (is.null(id)){
      message("No results.")
      return()
    }

    ind <- seq_len(min(max.results[i], length(ids)))
    if (return.sim.scores)
      scores <- xml_text(xml_find_all(temp, ".//LinkSetDb/Link/Score"))[ind]

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
#' @importFrom xml2 xml_text xml_find_all xml_attr
#' @noRd
ProcessPubMedResult <- function(tdoc){
  if (!length(tdoc))
     return()
  ## tdoc <- xmlDoc(article)
  res <- list()

  title <- xml_text(xml_find_all(tdoc,
                                 ".//MedlineCitation/Article/ArticleTitle"))
  res$title <- sub("\\.$", "", title, useBytes = FALSE)
  last.names <- xml_text(xml_find_all(tdoc,
                ".//MedlineCitation/Article/AuthorList/Author/LastName"))
  first.names <- xml_text(xml_find_all(tdoc,
                ".//MedlineCitation/Article/AuthorList/Author/ForeName"))
  res$author <- as.person(paste(first.names, last.names))
  if (!length(res$author)){
      last.names <- xml_text(xml_find_all(tdoc,
                ".//MedlineCitation/Article/AuthorList/Author/CollectiveName"))
      if (length(last.names))
          res$author <- person(last.names)
  }
  complete.AuthorList <- xml_attr(xml_find_all(tdoc,
                    ".//MedlineCitation/Article/AuthorList"),
                                    attr = "CompleteYN", default = "")

  res$year <- extractPubMedDatePart(tdoc, type = "Article", date.part = "Year")
  res$month <- extractPubMedDatePart(tdoc, type = "Article", date.part = "Month")

  res$journal <- xml_text(xml_find_all(tdoc,
                         ".//MedlineCitation/Article/Journal/Title"))

  res$volume <- xml_text(xml_find_all(tdoc,
                    ".//MedlineCitation/Article/Journal/JournalIssue/Volume"))

  res$number <- xml_text(xml_find_all(tdoc,
                    ".//MedlineCitation/Article/Journal/JournalIssue/Issue"))

  res$pages <- xml_text(xml_find_all(tdoc,
                        ".//MedlineCitation/Article/Pagination/MedlinePgn"))
  res$eprint <- xml_text(xml_find_all(tdoc, ".//MedlineCitation/PMID"))
  doi <- xml_text(xml_find_all(tdoc, ".//PubmedData/ArticleIdList/ArticleId"))
  res$doi <- if (length(doi > 1)){
    id.types <- xml_attr(xml_find_all(tdoc,
                    ".//PubmedData/ArticleIdList/ArticleId"),
                                    attr = "IdType", default = "")
    if (length(doi.pos <- grep("doi", id.types, ignore.case = TRUE,
                               useBytes = FALSE)))
       doi[doi.pos[1L]]
    else{
      doi <- vapply(doi, SearchDOIText, "")
      if (any(doi.pos <- vapply(doi, nzchar, FALSE)))
        doi[doi.pos[1L]]
      else
        NULL
    }
  }else
    doi
  # res$doi <- grep("/", doi, value = TRUE)

  res$language <- xml_text(xml_find_all(tdoc,
                           ".//MedlineCitation/Article/Language"))
  res$issn <- xml_text(xml_find_all(tdoc,
                           ".//MedlineCitation/Article/Journal/ISSN"))

  res$abstract <- xml_text(xml_find_all(tdoc,
                            ".//MedlineCitation/Article/Abstract/AbstractText"))
  ## some abstracts are separated into sections: methods, conclusion, etc.
  if (length(res$abstract) > 1){
      abstract.labs <- xml_attr(xml_find_all(tdoc,
                    ".//MedlineCitation/Article/Abstract/AbstractText"),
                                    attr = "Label", default = "")
      res$abstract <- if (!any(vapply(abstract.labs, .is_not_nonempty_text,
                                      FALSE)))
                          paste0(paste(abstract.labs, res$abstract,
                                       sep = ": "), collapse = "\n")
                      else paste0(res$abstract, collapse = "\n")
  }

  if (!length(complete.AuthorList) || complete.AuthorList == "N")
    warning(gettextf("Incomplete list of authors returned by PubMed for ID: %s",
                   res$eprint), call. = FALSE)

  ## free(tdoc)
  res$eprinttype <- "pubmed"

  attr(res, "entry") <- if (!is.null(res$journal) && !is.null(res$author))
                          "article"
                        else
                          "misc"

  attr(res, "key") <- CreateBibKey(res$title, res$author, res$year)
  MakeBibEntry(res, FALSE)
}

#' @importFrom xml2 xml_text xml_find_first
#' @noRd
extractPubMedDatePart <- function(nodes, type = "Article", date.part = "Year"){
  xpaths <-
    if (type == "Book")
      c("//PubmedBookArticle/BookDocument/Book/PubDate/",
        "//PubmedBookArticle/PubmedBookData/History/PubMedPubDate/",
        ".//PubmedBookArticle//")
    else
      c(".//MedlineCitation/Article/Journal/JournalIssue/PubDate/",
        ".//MedlineCitation/Article/ArticleDate/",
        ".//PubmedData/History/PubMedPubDate/",
        ".//MedlineCitation//", ".//PubmedData//")
  xpaths <- paste0(xpaths, date.part)

  out <- character(0)
  i <- 1
  while ((!length(out) || is.na(out)) && i <= length(xpaths)){
    out <- xml_text(xml_find_first(nodes, xpaths[i]))
    i <- i + 1
  }
  out
}

ProcessPubMedBookResult <- function(tdoc){
  if (!length(tdoc))
      return()
  res <- list()

  title <- xml_text(xml_find_all(tdoc,
                         ".//Book/BookTitle"))
  res$title <- gsub("\\.$", "", title, useBytes = FALSE)
  last.names <- xml_text(xml_find_all(tdoc,
        ".//Book/AuthorList/Author/LastName"))
  first.names <- xml_text(xml_find_all(tdoc,
        ".//Book/AuthorList/Author/ForeName"))
  res$author <- as.person(paste(first.names, last.names))

  res$year <- extractPubMedDatePart(tdoc, type = "Book", date.part = "Year")
  res$month <- extractPubMedDatePart(tdoc, type = "Book", date.part = "Month")

  res$booktitle <- xml_text(xml_find_all(tdoc,
                      ".//BookDocument/Book/CollectionTitle"))

  res$publisher <- xml_text(xml_find_all(tdoc,
          ".//BookDocument/Book/Publisher/PublisherName"))

  res$location <- xml_text(xml_find_all(tdoc,
          ".//BookDocument/Book/Publisher/PublisherLocation"))
  res$eprint <- xml_text(xml_find_all(tdoc,
                                      ".//BookDocument/PMID"))
  doc.ids <- xml_text(xml_find_all(tdoc,
                 ".//PubmedBookData/ArticleIdList/ArticleId"))
  id.types <- xml_attr(xml_find_all(tdoc,
                  ".//PubmedBookData/ArticleIdList/ArticleId"),
                                    attr = "IdType", default = "")
  ## res$eprint <- if(length(pmid.pos <- grep("pubmed", id.types,
  ##                                          ignore.case = TRUE)))
  ##                  doc.ids[pmid.pos[1L]]
  res$doi <- if (length(doi.pos <- grep("doi", id.types, ignore.case = TRUE,
                                        useBytes = FALSE)))
               doc.ids[doi.pos[1L]]
             else{
               doc.ids <- vapply(doc.ids, SearchDOIText, "")
               if (any(doi.pos <- vapply(doc.ids, nzchar, FALSE)))
                 doc.ids[doi.pos[1L]]
               else
                 NULL
             }

  # res$doi <- grep("/", doi, value = TRUE)

  res$language <- xml_text(xml_find_all(tdoc,
                           ".//BookDocument/Language"))
  res$abstract <- xml_text(xml_find_all(tdoc,
                    ".//BookDocument/Abstract/AbstractText"))
  ## some abstracts are separated into sections: methods, conclusion, etc.
  if (length(res$abstract) > 1L){
      abstract.labs <- xml_attr(xml_find_all(tdoc,
                    ".//BookDocument/Abstract/AbstractText"),
                                    attr = "Label", default = "")
      res$abstract <- if (!any(vapply(abstract.labs, .is_not_nonempty_text,
                                      FALSE)))
                          paste0(paste(abstract.labs, res$abstract, sep = ": "),
                                 collapse = "\n")
                      else paste0(res$abstract, collapse = "\n")
  }

  res$eprinttype <- "pubmed"

  attr(res, "entry") <- if (!is.null(res$booktitle))
                          "InBook"
                        else
                          "Book"

  attr(res, "key") <- CreateBibKey(res$title, res$author, res$year)

  return(MakeBibEntry(res, FALSE))
}

#' Retrieve PubMed ID's for a BibEntry object
#'
#' Uses the NCBI E-utilities to to search for PubMed ID's for citations
#' stored in a BibEntry object.
#' @param bib a bibentry object
#' @param index indices specifying which entries of \code{bib} will be
#' searched for.  If \code{missing}, all entries
#' are searched for.
#' @return a BibEntry object - \code{bib} with additional eprinttype and eprint
#' fields when the search is successful
#' for an entry.
#' @details For each entry a citation string is created using the fields
#' journaltitle/journal, date/year,
#'   volume, pages, and author; and these strings are then used to search the
#' NCBI database for PubMed ID's.
#'
#'   If an ID is found for an entry, the entry is updated so that the eprinttype
#' field is assigned the value
#'   \dQuote{pubmed} and the eprint field is assigned the ID.
#' @family pubmed
#' @importFrom httr GET content
#' @keywords database
#' @export
#' @examples
#' if (interactive() && !httr::http_error("https://eutils.ncbi.nlm.nih.gov/")){
#'   file.name <- system.file("Bib", "RJC.bib", package = "RefManageR")
#'   bib <- ReadBib(file.name)
#'   LookupPubMedID(bib[[101:102]])
#' }
LookupPubMedID <- function(bib, index){
  stopifnot(inherits(bib, "BibEntry"))
  if (missing(index))
    index <- seq_along(bib)

  base.url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
                     "ecitmatch.cgi?db=pubmed&retmode=xml&bdata=")
  cit.strings <- vapply(bib[index], MakeCitationString, "")

  .url <- paste0(base.url,
                paste0(cit.strings, collapse = "%0D"))
  results <- GET(.url)  # getURL(.url)
  results <- content(results)
  m <- gregexpr("KeY\\|(NOT_FOUND|[0-9]+)", results, useBytes = FALSE)
  res <- unlist(regmatches(results, m))
  res <- sub("KeY\\|", "", res, useBytes = FALSE)
  ind <- grep("[0-9]", res, useBytes = FALSE)
  if (length(ind)){
    message(paste0("Success for entries: ", paste0(index[ind], collapse=", ")))
    bib$eprint[index[ind]] <- res[ind]
    bib$eprinttype[index[ind]] <- "pubmed"
  }else{
    message("No PubMed ID\'s found")
  }

  return(bib)
}

#' @keywords internal
#' @noRd
#' @importFrom lubridate year
#' @importFrom utils URLencode
MakeCitationString <- function(bib.entry){
  first.page <- sub("-[0-9]+", "", bib.entry$pages, useBytes = FALSE)
  res <- paste(bib.entry$journal, year(bib.entry$dateobj), bib.entry$volume,
               first.page, paste0(as.character(bib.entry$author),
                                  collapse = " "), "KeY", sep = "|")
  res <- URLencode(res)
  res <- gsub("%7C", "|", res, useBytes = FALSE)
  res <- gsub("%26", "and", res, useBytes = FALSE)
  res <- paste0(res, "|")
  return(gsub("%20", "+", res, useBytes = FALSE))
}
