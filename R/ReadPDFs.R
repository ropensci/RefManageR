#' Create bibliographic information from PDF Metadata.
#'
#' This function creates bibliographic information by reading the Metadata and text of PDFs stored in a user
#' specified directory using Poppler (\url{http://poppler.freedesktop.org/}).  IF requested, the function
#' first searches for DOIs and downloads \code{BibTeX} entries from \code{\link{ReadCrossRef}} if DOIs are
#' found.  If this is not requrested or a DOI is not found for an entry, an attempt is made to build a BibTeX
#' entry from the metadata and text.
#' @param path character; path to directory containing pdfs or filename of one pdf. \code{normalizePath} is
#'   used on the specified path
#' @param .enc character; text encoding to use for reading pdf and creating BibEntry object. Available
#'   encodings for Poppler can be found using \code{system("pdfinfo -listenc")}.  The encoding must
#'   also be listed in \code{iconvlist()}.
#' @param recursive logical; same as \code{\link{list.files}}.  Should pdfs in subdirectories of path be used?
#' @param use.crossref logical; should an attempt be made to download bibliographic information from CrossRef if
#' any Document Object Identifiers (DOIs) are found?
#' @param use.metadata logical; should the PDF metadata also be used to help create entries?
#' @param progress logical; should progress bar be generated when fetching from CrossRef?
#' @export
#' @details This function requires that the \code{pdfinfo} utility from Poppler PDF
#' \url{http://poppler.freedesktop.org/} be installed.
#'
#' This function will create only \code{Article} or \code{Misc} \code{BibTeX} entries.
#'
#' The absolute path to each file will be stored in the bib entry in a field called \sQuote{file}, which is
#' recognized by \code{BibLaTeX} (though not printed by any standard style) and can be used by the
#' \code{\link{open.BibEntry}} function to open the PDF in the default viewer.
#'
#' If the keywords metadata field is available, it will be added to the bib entry in a field
#' \sQuote{keywords}, which is recognized by \code{BibLaTeX}.
#' @return An object of class BibEntry.
#' @references \url{http://poppler.freedesktop.org/}
#' @keywords utilities
#' @seealso \code{\link{ReadCrossRef}}, \code{\link{BibEntry}}, \code{\link{open.BibEntry}}
#' @importFrom plyr llply progress_text
#' @importFrom utils flush.console
ReadPDFs <- function (path, .enc = 'UTF-8', recursive = TRUE, use.crossref = TRUE,
                      use.metadata = TRUE, progress = FALSE) {
  if (!nzchar(Sys.which("pdfinfo")))
     stop("poppler does not seem to be installed")
  path <- file.path(path[1])
  if (!file.exists(path))
      stop("Specified path does not exist")
  files <- list.files(path, pattern = '.pdf$', full.names = TRUE, recursive = recursive)
  if (!length(files)){  # check if directory or file specified
      if (!grepl("[.]pdf$", path, useBytes = TRUE))
          stop(gettextf("%s must be a valid path containing PDFs or a file name ending in %s",
                        sQuote("path"), dQuote(".pdf"), domain = NA))
    files <- path
  }
  n.files <- length(files)
  not.done <- seq.int(n.files)
  dois <- NULL

  #########################################################################
  # read metadata and search for DOI                                      #
  #########################################################################
  if (use.metadata){
    message(paste0('Getting Metadata for ',length(files), ' pdfs...'))
    flush.console()
    out <- lapply(files, function(x) system2("pdfinfo", paste(shQuote('-enc'), shQuote(.enc),
                                              shQuote(normalizePath(x))), stdout = TRUE, stderr = TRUE))

    dois <- sapply(out, SearchDOIText)
    doi.meta.ind <- !is.na(dois)
  }else
    doi.meta.ind <- logical(n.files)

  ########################################
  # search first two pages of pdf for DOI
  ########################################
  GetPDFTxt <- function(tpath, page, tfile, enc){
    system2("pdftotext", paste(shQuote('-f'), shQuote(page), shQuote('-l'), shQuote(page),
                               shQuote('-enc'), shQuote(.enc),
                               shQuote(normalizePath(tpath)), shQuote(tfile)))
    suppressWarnings(readLines(tfile, encoding = enc))
  }

  tfile1 <- tempfile(fileext = '.txt')
  txt.files1 <- lapply(files, GetPDFTxt, page = 1, tfile = tfile1, enc = .enc)
  txt.files2 <- lapply(files, GetPDFTxt, page = 2, tfile = tfile1, enc = .enc)
  file.remove(tfile1)

  # check first page for JSTOR, if yes grab info from both pages, else NA
  resJSTOR <- mapply(CheckJSTOR, txt.files1, txt.files2, files, SIMPLIFY = FALSE)
  JSTOR.ind <- !is.na(resJSTOR)
  done.inds <- which(JSTOR.ind)
  if(any(JSTOR.ind)){
    not.done <- not.done[!JSTOR.ind]
  }
  resJSTOR <- resJSTOR[JSTOR.ind]

  tind <- !JSTOR.ind & !doi.meta.ind
  # get DOIs and read from CrossRef
  if (length(tind)){
    more.dois <- mapply(function(p1, p2){
      SearchDOIText(c(p1,p2))
      }, txt.files1[tind], txt.files2[tind])

    doi.text.ind <- which(tind)[!is.na(more.dois)]
  }

  # don't call CrossRef if JSTOR already got info (need to check if DOI from metadata and JSTOR)
  metadoi.pos <- which(doi.meta.ind)
  doi.ind <- c(metadoi.pos[!metadoi.pos %in% JSTOR.ind], doi.text.ind)
  comb.doi <- c(dois[!JSTOR.ind & doi.meta.ind], more.dois[!is.na(more.dois)])

  # get bib info from CrossRef
  resCR <- CR.ind <- badCR.ind <- NULL
  if (use.crossref && length(doi.ind)){
    message(paste0('Getting ', length(comb.doi), ' BibTeX entries from CrossRef...'))
    flush.console()
     if (progress){
       progress <- progress_text(char = ".")
     }else{
       progress <- "none"
     }

     tmpbib <- tempfile(fileext = ".bib", tmpdir=getwd())
     resCR <- llply(as.list(comb.doi), ReadCrossRef, temp.file = tmpbib, delete.file = TRUE,
                    .progress = progress)
    message('Done')
    CR.ind <- !sapply(resCR, is.null)  # on very rare instances CrossRef doesn't have record for particular DOI
    badCR.ind <- doi.ind[!CR.ind]
    if(any(CR.ind)){
       for (i in which(CR.ind))
         resCR[[i]]$file <- files[doi.ind[i]]
      not.done <- not.done[!not.done %in% doi.ind[CR.ind]]
    }

    resCR <- MakeCitationList(resCR[CR.ind])  # llply returns a list of BibEntry objs, NOT single BibEntry obj
  }

  # get bib info from first page. if dont find abstract on first page, use second page too
  res <- lapply(txt.files1[not.done], ReadFirstPages, page.one = TRUE)
  # not.done <- not.done[is.na(res) || !res$found.abstract]
  res2 <- lapply(txt.files2[not.done], ReadFirstPages, page.one = FALSE)

  resMeta <- NULL
  if (use.metadata && length(not.done)){
   resMeta <- lapply(out[not.done], ProcessPDFMeta, enc=.enc)
   res <- mapply(CleanAuthorTitle, res, res2, resMeta, files[not.done], SIMPLIFY = FALSE)
  }else{
   res <- mapply(CleanAuthorTitle, res, res2, file = files[not.done],
                 MoreArgs = list(bibMeta=NULL), SIMPLIFY = FALSE)
  }
  #               JSTOR        ReadPDF+Meta           CrossRef
  done.inds <- c(done.inds, not.done[!is.na(res)], doi.ind[CR.ind])
  not.done <- not.done[is.na(res)]
  res <- res[!is.na(res)]  # remove entries with no author or title info - done in MakeCitationList

  res <- c(resJSTOR, res)
  if (length(res)){
    res <- withCallingHandlers( lapply(res, MakeBibEntry, to.person = FALSE), warning = function(w){
      if( any( grepl("recycled", w$message, useBytes = TRUE) ) )
        invokeRestart( "muffleWarning" )
      })
    res <- MakeCitationList(res)
  }

  if (length(resCR))
    res <- suppressWarnings(c(resCR, res))  # suppressWarnings in case res NULL


  # add file names and dois as fields (if doi not done already)
  if (length(not.done) < length(res)){
    if (!use.crossref && length(doi.ind)){
      ind <- which(done.inds %in% doi.ind)
      ind2 <- which(doi.ind %in% done.inds)
      for (i in seq_along(ind))
        res[[ind[i]]]$doi <- comb.doi[ind2[i]]
    }else if (use.crossref && length(badCR.ind)){  # DOI's missed by CrossRef may be fixed up later
      ind <- which(done.inds %in% badCR.ind)
      ind2 <- which(doi.ind %in% badCR.ind)
      for (i in seq_along(ind))
        res[[ind[i]]]$doi <- comb.doi[ind2[i]]
    }
  }
  res <- MakeKeysUnique(res)
  return(res)
}
