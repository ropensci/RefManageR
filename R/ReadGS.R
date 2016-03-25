#' Import book and article references from a public Google Scholar profile by ID.
#'
#' This function will create a BibEntry object for up to 100 references from a provided Google Scholar ID,
#' if the profile is public.  The number of citations for each entry will also be imported.
#'
#' @param scholar.id character; the Google Scholar ID from which citations will be imported.  The ID can by found by
#' visiting an author's Google Scholar profile and noting the value in the uri for the \dQuote{user} parameter.
#' @param start numeric; index of first citation to include.
#' @param limit numeric; maximum number of results to return.  Cannot exceed 100.
#' @param sort.by.date boolean; if true, newest citations are imported first; otherwise, most cited works are imported first.
#' @param .Encoding character; text encoding to use for importing the results and creating the bib entries.
#' @param check.entries What should be done with incomplete entries (those containing \dQuote{...} due to long fields)?
#' Either \code{FALSE} to add them anyway, \code{"warn"} to add with a warning, or any other value to drop the entry
#' with a message and continue processing the remaining entries.
#' @importFrom RCurl getForm curlEscape
#' @importFrom XML xpathApply htmlParse
#' @importFrom httr GET
#' @importFrom stringr str_length str_sub
#' @keywords database
#' @export
#' @details This function creates \code{BibTeX} entries from an author's
#' Google Scholar page.
#' If the function finds numbers corresponding to volume/number/pages of a journal
#' article, an \sQuote{Article} entry
#' is created.  If an arXiv identifier is found, a \sQuote{Misc} entry is created
#' with \code{eprint}, \code{eprinttype}, and \code{url} fields.  Otherwise, a
#' \sQuote{TechReport} entry is created; unless the entry has more than ten citations,
#' in which case a \sQuote{Book} entry is created.
#'
#' Long author lists, long titles, and long journal/publisher names can all lead to these fields being incomplete for
#' a particular entry.  When this occurs, these entries are either dropped or added with
#' a warning depending on the value
#' of the \code{check.entries} argument.
#'
#' @return An object of class BibEntry.  If the entry has any citations, the number of
#' citations is stored in a field \sQuote{cites}.
#' @seealso \code{\link{BibEntry}}
#' @note Read Google's Terms of Service before using.
#'
#' It is not possible to automatically import BibTeX entries directly from Google
#' Scholar as no API is available and this violates their Terms of Service.
#' @examples
#' if (interactive() && url.exists("http://scholar.google.com")){
#'   ## R. J. Carroll's ten newest publications
#'   ReadGS(scholar.id = "CJOHNoQAAAAJ", limit = 10, sort.by.date = TRUE)
#'
#'   ## Matthias Katzfu\ss
#'   BibOptions(check.entries = "warn")
#'   kat.bib <- ReadGS(scholar.id = "vqW0UqUAAAAJ")
#'
#'   ## retrieve GS citation counts stored in field 'cites'
#'   kat.bib$cites
#' }
ReadGS <- function(scholar.id, start = 0, limit = 100, sort.by.date = FALSE,
                     .Encoding = "UTF-8", check.entries = BibOptions()$check.entries){
  limit <- min(limit, 100)
  ps <- ifelse(limit <= 20, 20, 100)
  oldvio <- BibOptions(check.entries = check.entries)
  on.exit(BibOptions(check.entries = oldvio))

  .params <- list(hl = "en", user = scholar.id, oe = .Encoding, pagesize = ps,
                  view_op = "list_works", cstart = start)
  if (sort.by.date)
    .params$sortby = "pubdate"

  uri <- "http://scholar.google.com/citations"
  els <- mapply(function(id, val) {
      paste(curlEscape(id), curlEscape(val), sep = "=", collapse = "&")
  }, names(.params), .params)
  args <- paste(els, collapse = "&")
  uri <- paste(uri, args, sep = if (grepl("\\?", uri, useBytes = TRUE))
                                  "&"
                                else "?")

  doc <- GET(uri)
  ## doc <- htmlParse(uri)

  ## doc <- GetForm(uri, .params = .params, .encoding = .Encoding)
  cites <- xpathApply(htmlParse(doc, encoding = .Encoding),
                       "//tr[@class=\"gsc_a_tr\"]")  # "//tr[@class=\"cit-table item\"]")
  ## cites <- xpathApply(doc, "//tr[@class=\"gsc_a_tr\"]")
  if(!length(cites)){
    message("no results")
    return()
  }
  cites <- cites[seq_len(min(limit, length(cites)))]

  noNAwarn <- function(w) if( any( grepl( "NAs introduced", w, useBytes = TRUE) ) )
      invokeRestart( "muffleWarning" )
  tmp <- withCallingHandlers(lapply(cites, ParseGSCites2), warning = noNAwarn)
  out <- lapply(tmp[!is.na(tmp)], MakeBibEntry, to.person = FALSE)
  out <- MakeCitationList(out)

  out
}
