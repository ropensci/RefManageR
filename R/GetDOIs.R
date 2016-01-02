#' Search CrossRef for Document Object Identifiers for Given Citations
#'
#' This function queries CrossRef to obtain DOIs for the entries in a given
#' \code{BibEntry} object.
#' @param bib a \code{BibEntry} object
#' @return \code{bib} with any found DOI's added in the \sQuote{doi} field
#' @importFrom RJSONIO toJSON fromJSON
#' @importFrom RCurl postForm
#' @keywords database
#' @export
#' @seealso \code{\link{ReadCrossRef}}
#' @family pubmed
#' @note Only entries in \code{bib} that do not already contain a value in the
#' \sQuote{doi} field will be searched for.
#' @references \url{http://search.crossref.org/help/api}
#' @examples
#' if (interactive() && url.exists("http://search.crossref.org")){
#'   BibOptions(check.entries = FALSE, sorting = "none")
#'   bib <- ReadBib(system.file("Bib", "RJC.bib", package = "RefManageR"))[1:5]
#'   bib <- GetDOIs(bib)
#'   bib$doi
#' }
GetDOIs <- function(bib){
  missing.dois.pos <- which(if (length(bib) == 1)
                        is.null(bib$doi)
                      else sapply(bib$doi, is.null))
  if (!length(missing.dois.pos))
    message("All entries already have DOIs")
  else{
    json.bib <- toJSON(FormatEntryForCrossRef(bib[[missing.dois.pos]]))
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
                                                          "doi")[matches], useBytes = TRUE)
      }
    }
  }
  bib
}


FormatEntryForCrossRef <- function(bib){
    fmt.env <- MakeBibLaTeX("text", TRUE)
    assign("bib", bib, envir = fmt.env)
    with(fmt.env, {
      bibstyle <- "authoryear"
      collapse <- function(strings)
                    paste(strings, collapse = " ")

      fmtVolume <- function(vol, num){
          if (length(vol)){
            res <- paste0("vol. ", vol)
            if (length(num))
              res <- paste(res, num, sep = ', no. ')
            res
          }
       }
       fmtJTitle <- function(title){
         if (grepl('[.?!]$', title, useBytes = TRUE))
           paste0("\"", collapse(cleanupLatex(title)), "\"")
         else paste0("\"", collapse(cleanupLatex(title)), "\".")
       }
      
      fmtJournal <- function(s){
        if (length(s$journaltitle)){
          res <- cleanupLatex(s$journaltitle)
          if (length(s$journalsubtitle))
            res <- paste(addPeriod(res), cleanupLatex(s$journalsubtitle))
          return(res)
        }else if(!is.null(s$journal)){
          cleanupLatex(s$journal)  
        }
      }

      formatArticle <- function(paper){
         collapse(c(fmtBAuthor(paper), fmtJTitle(paper$title), 
               sentence(fmtJournal(paper), fmtVolume(paper$volume, paper$number),
                                      fmtPages(paper$pages, paper$pagination),
                       fmtDate(attr(paper, "dateobj")), sep = ', '),
               fmtISSN(paper$issn)))
      }
      oldopts <- BibOptions(max.names = 99, first.inits = TRUE)
      on.exit(BibOptions(oldopts))
      sapply(unclass(bib), function(doc){
            doc$.duplicated <- FALSE
            formatArticle(doc)
          })
      })
}
