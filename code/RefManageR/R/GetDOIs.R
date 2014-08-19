#' Search CrossRef for Document Object Identifiers for Given Citations
#'
#' This function queries CrossRef to obtain DOIs for the entries in a given
#' \code{BibEntry} object.
#' @param bib a \code{BibEntry} object
#' @return \code{bib} with any found DOI's added in the \sQuote{doi} field
#' @importFrom RJSONIO toJSON fromJSON
#' @importFrom RCurl postForm
#' @keywords database
#' @seealso \code{\link{ReadCrossRef}}
#' @family pubmed
#' @note Only entries in \code{bib} that do not already contain a value in the
#' \sQuote{doi} field will be searched for.
#' @references \url{http://search.crossref.org/help/api}
#' @examples
#' if (interactive() && url.exists("http://search.crossref.org")){
#'   BibOptions(check.entries = FALSE)
#'   bib <- ReadBib(system.file("Bib", "RJC.bib", package = "RefManageR"))[1:5]
#'   bib <- GetDOIs(bib)
#'   bib$doi
#' }
GetDOIs <- function(bib){
  missing.dois.pos <- which(sapply(bib$doi, is.null))
  if (!length(missing.dois.pos))
    message("All entries already have DOIs")
  else{
    json.bib <- toJSON(format(bib[[missing.dois.pos]], style = "text", .sort = FALSE))

    headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json')
    json.res <- postForm("http://search.crossref.org/links",
                      .opts = list(postfields = json.bib, httpheader = headers))
    json.res <- try(fromJSON(json.res), TRUE)
    if (inherits(json.res, "try-error") || json.res[[2]] == FALSE)
      message("Error with query.")
    else{
      matches <- sapply(json.res[[1]], "[[", "match")
      if (!any(matches))
        message("No matches.")
      else{
        message(paste0("Matches for entries at positions ",
                       paste0(missing.dois.pos[matches], collapse = ", "), "."))
        bib$doi[missing.dois.pos[matches]] <- sub("http://dx.doi.org/", "",
                                                   sapply(json.res[[1]], "[[",
                                                          "doi")[matches])
      }
    }
  }
  bib
}

