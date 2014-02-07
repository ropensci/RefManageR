# Mathew McLean
# October 26, 2013
# function for reading from Zotero library

############################################################################################################
# zotero: name=mwmclean, key=7lhgvcwVq60CDi7E68FyE3br userid for API= 1648676; collection key = 3STEQRNU
############################################################################################################
# http://www.zotero.org/support/kb/ssl_certificate_error
# names(getCurlOptionsConstants())
# http://www.zotero.org/support/dev/server_api/v2/write_requests
# 
# http://www.omegahat.org/RCurl/philosophy.html
# .opts explained: http://curl.haxx.se/libcurl/c/curl_easy_setopt.html

#' Get Bibliography Information From a Zotero Library.
#' 
#' @param user Zotero userID for use in calls to the Zotero API.  This is not the same as your Zotero 
#'   username.  The userID for accessing user-owned libraries can be found at 
#'   \url{https://www.zotero.org/settings/keys}.
#' @param .params A \emph{named} list of parameters to use in requests to the Zotero API with possible values
#'  \itemize{
#'    \item q - Search string to use to search the library
#'    \item qmode - Search mode. Default is "titleCreatorYear".  Use "everything" to include full-text content in search.
#'    \item key - API key.  This must be specified to access non-public libraries.
#'    \item collection - name of a specific collection within the library to search
#'    \item itemType - type of entry to search for; e.g., "book" or "journalArticle"
#'    \item tag - name of tag to search for in library
#'    \item limit - maximum number of entries to return
#'    \item start - index of first entry to return
#'  }
#' @param temp.file character; file name where the BibTeX data returned by Zotero will be temporarily written.
#' @param delete.file boolean; should \code{temp.file} be removed on exit?
#' @return An object of class BibEntry
#' @seealso \code{\link{BibEntry}}, \code{\link{getForm}} in package \code{RCurl}
#' @references \url{http://www.zotero.org/support/dev/server_api/v2/read_requests}
#' @importFrom RCurl getForm curlOptions
#' @examples
#' ## first two entries in library with bayesian in title
#' ReadZotero(user='1648676', .params=list(q='bayesian', key='7lhgvcwVq60CDi7E68FyE3br', limit=2))
#'
#' ## Search specific collection
#' ## collection key can be found by reading uri when collection is selected in Zotero
#' ReadZotero(user='1648676', .params=list(q='yu', key='7lhgvcwVq60CDi7E68FyE3br', collection='3STEQRNU'))
#'
#' ## Search by tag
#' ## Notice issue with how Zotero uses Techreport entry for arXiv manuscripts
#' ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))
#' @keywords database
ReadZotero <- function(user, .params, temp.file = tempfile(fileext = '.bib'), delete.file = TRUE){
  if (delete.file)
    on.exit(unlink(temp.file, force = TRUE))
  
  bad.ind <- which(!names(.params) %in% c('q', 'itemType', 'tag', 'collection', 'key', 'limit', 'start', 'qmode'))
  .parms <- .params
  if(length(bad.ind)){
    warning('Invalid .params specified and will be ignored')
    .parms <- .parms[-bad.ind]
  }
  .parms$format <- 'bibtex'
  if(is.null(.parms$limit))
    .parms$limit <- 99
  
  if(is.null(.params$collection)){
    .parms$uri <- paste('https://api.zotero.org/users/', user, '/items', sep='')
  }else{
    .parms$uri <- paste('https://api.zotero.org/users/', user, '/collections/', .params$collection, '/items', sep='')
  }
  
  cert <- try(system.file("CurlSSL/cacert.pem", package = "RCurl"))
  if(class(cert)=='try-error'){
    .parms$.opts <- curlOptions(ssl.verifypeer=FALSE, httpheader='Zotero-API-Version: 2', forbid.reuse=TRUE)
  }else{
    .parms$.opts <- curlOptions(ssl.verifypeer=TRUE, httpheader='Zotero-API-Version: 2', forbid.reuse=TRUE, cainfo=cert)
  }
  
  res <- do.call(getForm, .parms)
                  
  if(class(res)=='raw') # not sure why this happens. happens with getForm sometimes, but not with getURL for same uri
    res <- rawToChar(res)
  
  if(res==''){
    print('No results.')
    return()
  }
  write(res, file=temp.file)
                  
  ReadBib(temp.file)
}

# # GOOD
# test <- getForm(URL, q="mclean", itemType="book || journalArticle", format="bib?",
#                  .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                      forbid.reuse=TRUE))  
# test <- mygetForm(URL, format="bibtex", limit=5, 
#                  .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                      forbid.reuse=TRUE))
# # GOOD
#                "https://api.zotero.org/users/1648676/items?key=7lhgvcwVq60CDi7E68FyE3br&format=bibtex&limit=5"
# test <- getURL('https://api.zotero.org/users/1648676/items?key=7lhgvcwVq60CDi7E68FyE3br&format=bibtex&limit=5&q=mcASD',
#                .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                  forbid.reuse=TRUE))  
# test <- getURL('https://api.zotero.org/users/1648676/collections/3STEQRNU/items?key=7lhgvcwVq60CDi7E68FyE3br&format=bibtex&limit=5',
#                .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                  forbid.reuse=TRUE))  
# test <- getURL('https://api.zotero.org/users/1648676/collections?key=7lhgvcwVq60CDi7E68FyE3br',
#                .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                  forbid.reuse=TRUE))  
# # list(q='mclean', itemType='journal', 'tag', 'collection', 'key')
# testRZ <- ReadZotero(user='1648676', .params=list(q='yu', key='7lhgvcwVq60CDi7E68FyE3br', collection='3STEQRNU'))
# test <- ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))
# 
# 
# 
# test[2]$journal

##############################

# d <- debugGatherer()
# URL <- "https://api.zotero.org/users/1648676/items?key=7lhgvcwVq60CDi7E68FyE3br"  # "https://api.zotero.org/users/1648676/keys/7lhgvcwVq60CDi7E68FyE3br"
# test <- getForm(URL, q="carroll measurement error", itemType="itemType=book || journalArticle", format="bibtex",
#                 .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE), debugfunction=d,
#                 httpheader = c("User-Agent" = "R (3.0.1)"))
# d$value()
# # URL <- "https://api.zotero.org/users/1648676/keys/7lhgvcwVq60CDi7E68FyE3br"
# cert <- system.file("CurlSSL/cacert.pem", package = "RCurl")
# test <- getForm(URL, q="mclean", itemType="book || journalArticle", format="bib",
#                 .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                   forbid.reuse=TRUE))
# URLl <- paste(URL, '?q=mclean', sep='')
# test <- getURL(URLl,
#                .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                  forbid.reuse=TRUE))
# dd <- xmlParse(test)
# links <- getNodeSet(dd)
# test2 <- getForm(URL, q="kzrt", #itemType="itemType=book || journalArticle", format="bib",
#                  .opts=curlOptions(ssl.verifypeer=FALSE, verbose=TRUE, cainfo=cert, httpheader="Zotero-API-Version: 2",
#                                    forbid.reuse=TRUE))
# dd2 <- htmlParse(test2)
# #httpheader = c("Zotero-API-Version: 2"))
# # test <- getURL("https://api.zotero.org/users/1648676/keys/7lhgvcwVq60CDi7E68FyE3br/format=bib", 
# #               .opts=list(ssl.verifypeer=TRUE, verbose=TRUE, cainfo=cert, httpheader = c('Zotero-API-Version: 2')))
# # debugfunction=d,
# #httpheader = c("User-Agent" = "R (3.0.1)",'Zotero-API-Version: 2')
# 
# )
# 
# test <- getURL("https://api.zotero.org/users/1648676/keys/7lhgvcwVq60CDi7E68FyE3br", cainfo=cert, 
#                .opts=list(ssl.verifypeer=TRUE, verbose=TRUE),  # debugfunction=d,
#                httpheader = c("User-Agent" = "R (3.0.1)")  # header='Zotero-API-Version: 2',
# )
# dd <- xmlParse(test)
# dd <- htmlParse(test)
# links <- getNodeSet(dd, "//a[@href]")
