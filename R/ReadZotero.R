# Mathew McLean
# October 26, 2013
# function for reading from Zotero library

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
#'   \verb{https://www.zotero.org/settings/keys} after logging in.
#' @param group Zotero groupID for use in calls to the Zotero API.  Only one of \code{user} and \code{group} should be specified; \code{group} will be ignored if both are specified.
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
#' @export
#' @seealso \code{\link{BibEntry}}, \code{\link{getForm}} in package \code{RCurl}
#' @references \verb{http://www.zotero.org/support/dev/server_api/v2/read_requests}
#' @importFrom RCurl getForm curlOptions
#' @examples
#' \dontrun{
#' ## first two entries in library with bayesian in title
#' ReadZotero(user = "1648676", .params = list(q = "bayesian",
#'   key = "7lhgvcwVq60CDi7E68FyE3br", limit=2))
#'
#' ## Search specific collection
#' ## collection key can be found by reading uri when collection is selected in Zotero
#' ReadZotero(user = "1648676", .params=list(q = "yu", key = "7lhgvcwVq60CDi7E68FyE3br",
#'   collection = "3STEQRNU"))
#'
#' ## Search by tag
#' ## Notice the issue with how Zotero uses a TechReport entry for arXiv manuscripts
#' ## This is one instance where the added fields of BibLaTeX are useful
#' ReadZotero(user = "1648676", .params=list(key = "7lhgvcwVq60CDi7E68FyE3br",
#'   tag = "Statistics - Machine Learning"))
#'
#' ## To read these in you must set check.entries to FALSE or "warn"
#' old.opts <- BibOptions(check.entries = FALSE)
#' length(ReadZotero(user = "1648676", .params = list(key = "7lhgvcwVq60CDi7E68FyE3br",
#'   tag = "Statistics - Machine Learning")))
#'
#' ## Example using groups
#' ReadZotero(group = "13495", .params = list(q = "Schmidhuber",
#'   collection = "QU23T27Q"))
#' BibOptions(old.opts)
#' }
#' @keywords database
ReadZotero <- function(user, group, .params, temp.file = tempfile(fileext = '.bib'), delete.file = TRUE){
  if (delete.file)
    on.exit(unlink(temp.file, force = TRUE))

  bad.ind <- which(!names(.params) %in% c('q', 'itemType', 'tag', 'collection', 'key', 'limit', 'start', 'qmode'))
  .parms <- .params
  if (length(bad.ind)){
    warning("Invalid .params specified and will be ignored")
    .parms <- .parms[-bad.ind]
  }
  .parms$format <- 'bibtex'
  if (is.null(.parms$limit))
    .parms$limit <- 99L

  coll <- if (is.null(.parms$collection))
            NULL
          else
            paste0("/collections/", .parms$collection)
  .parms$uri <- paste0("https://api.zotero.org/", if (!missing(user))
                        paste0("users/", user) else paste0("groups/", group),
                       coll, "/items")

  cert <- try(system.file("CurlSSL/cacert.pem", package = "RCurl"))
  .parms$.opts <- if (class(cert)=='try-error')
      curlOptions(ssl.verifypeer=FALSE, httpheader='Zotero-API-Version: 2',
                  forbid.reuse=TRUE)
  else
    curlOptions(ssl.verifypeer=TRUE, httpheader='Zotero-API-Version: 2',
               forbid.reuse=TRUE, cainfo=cert)

  res <- do.call(getForm, .parms)
  ## not sure why this happens. happens with getForm sometimes, but
  ##   not with getURL for same uri
  if (class(res)=='raw')
    res <- rawToChar(res)

  if (res == ''){
    print('No results.')
    return()
  }
  write(res, file=temp.file, append = TRUE)

  bib.res <- try(ReadBib(file=temp.file, .Encoding='UTF-8'), TRUE)
  if (inherits(bib.res, "try-error")){
    message(paste0("Error parsing the returned BibTeX results.  If delete.file ",
                   "is FALSE, you can try viewing and editing the file: ", temp.file))
    return()
  }
  bib.res
}
