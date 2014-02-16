# things to test: 
# DOI found - crossref doesn't work
# use.crossref = FALSE, use.metadata = TRUE
# vice versa
# use.crossref = TRUE and no DOIs found
# no results
# only DOI res no others
# both FALSE
# error downloading fro


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
ReadPDFs <- function (path, .enc = 'UTF-8', recursive = TRUE, use.crossref = TRUE, use.metadata = TRUE) {
  #outfile <- tempfile("pdfinfo")
  # browser()
#   if (delete.file)
#     on.exit(unlink(temp.file))
#  files1 <- files2 <- files3 <- NULL
  
  files <- list.files(path, pattern = '.pdf$', full.names = TRUE, recursive = recursive)
  if (!length(files))  # check if directory or file specified
    files <- path
  
#  out <- lapply(files, function(x) system(paste(shQuote("pdfinfo"), shQuote('-enc'), shQuote(encoding), 
#                                                              shQuote(normalizePath(x))), intern = TRUE)) 
  n.files <- length(files)
  not.done <- seq.int(n.files)
  dois <- NULL
  
#   FullInd <- function(in.ind, out){
#     res <- logical(length(in.ind))
#     res[in.ind] <- is.na(out)
#     res
#   }
 # browser()
  
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
    # not.done <- which(is.na(dois))
  }

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
  resJSTOR <- mapply(CheckJSTOR, txt.files1, txt.files2, files)
  JSTOR.ind <- !is.na(resJSTOR)
  done.inds <- which(JSTOR.ind)
  if(any(JSTOR.ind)){
  #  for (i in which(JSTOR.ind))
  #    resJSTOR[[i]]$file <- files[i]
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
    # res <- lapply(dois, ReadCrossRef)
#     resCR <- vector('list', length(doi.ind))
#      for (i in 1:length(doi.ind))
#        resCR[[i]] <- unclass(ReadCrossRef(comb.doi[i], temp.file = tmpbib, delete.file = FALSE))
     tmpbib <- tempfile(fileext = ".bib", tmpdir=getwd())
     resCR <- llply(as.list(comb.doi), ReadCrossRef, temp.file = tmpbib, delete.file = TRUE,
                    .progress = progress_text(char = "."))
#     resCR <- llply(as.list(comb.doi), .fun = function(x, tmpfile){
#       rcrres <- ReadCrossRef(x, temp.file=tmpfile, delete.file=FALSE)
#       return(unclass(rcrres))
#       }, tmpfile = tmpbib, .progress = progress_text(char = "."))
    #file.remove(tmpbib)  
    message('Done')
    CR.ind <- !sapply(resCR, is.null)  # is.na(resCR)      # on very rare instances CrossRef doesn't have record for particular DOI
    badCR.ind <- doi.ind[!CR.ind]
    if(any(CR.ind)){
       for (i in which(CR.ind))
         resCR[[i]]$file <- files[doi.ind[i]]
      not.done <- not.done[!not.done %in% doi.ind[CR.ind]]
    }
    
    resCR <- MakeCitationList(resCR[CR.ind])  # llply returns a list of BibEntry objs, NOT single BibEntry obj
   # class(res) <- c('Bibentry', 'bibentry')
   #  not.done <- not.done[-doi.ind[CR.ind]] # seq.int(n.files)[-which(not.na1)[!is.na(res)]]
  }
  
  # get bib info from first page. if dont find abstract on first page, use second page too
  res <- lapply(txt.files1[not.done], ReadFirstPages, page.one = TRUE)
  # not.done <- not.done[is.na(res) || !res$found.abstract]
  res2 <- lapply(txt.files2[not.done], ReadFirstPages, page.one = FALSE)
  

 # browser()
  resMeta <- NULL
  if (use.metadata && length(not.done)){
   #message(paste0('Attempting to create BibTeX entries from PDF metadata for ', length(not.done), ' entries...'))
   resMeta <- lapply(out[not.done], ProcessPDFMeta, enc=.enc)                
   #message('Done.')
   #flush.console()
  # na3 <- is.na(res3)
   #res3 <- res3[!na3]
  # res <- AddListToList(res, resMeta)
  # files3 <- files[!na3]
   # not.done <- not.done[is.na(res2)]
   #res2 <- lapply(res2, MakeBibEntry, GS = TRUE)
   #res2 <- MakeCitationList(res2)
   #res2 <- c(res2, res3)
   res <- mapply(CleanAuthorTitle, res, res2, resMeta, files[not.done], SIMPLIFY = FALSE)
  }else{
   res <- mapply(CleanAuthorTitle, res, res2, files[not.done], 
                 MoreArgs = list(resMeta=NULL), SIMPLIFY = FALSE) 
  }
  #               JSTOR        ReadPDF+Meta           CrossRef
  done.inds <- c(done.inds, not.done[!is.na(res)], doi.ind[CR.ind])
  not.done <- not.done[is.na(res)]
  res <- res[!is.na(res)]  # remove entries with no author or title info - done in MakeCitationList

  res <- c(resJSTOR, res)
  if (length(res)){
    res <- withCallingHandlers( lapply(res, MakeBibEntry, to.person = FALSE), warning = function(w){
      if( any( grepl("recycled", w$message) ) ) 
        invokeRestart( "muffleWarning" ) 
      })
    res <- MakeCitationList(res)
  }
  
  if (length(resCR))
    res <- suppressWarnings(c(resCR, res))  # suppressWarnings in case res NULL

  
  # add file names and dois as fields (if doi not done already)
  if (length(not.done) < length(out)){
    # res$file[done.inds] <- files[done.inds]
    # browser()
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
  
  #res <- MakeCitationList(res)
  
  return(res)
}

# meta <- ReadPDFMeta('~/NewPapers/')
# 
# reader <- readPDF(PdftotextOptions = "-layout")
# doc <- reader(elem = list(uri = '~/New Papers/BootstrappingClusteredData(JRSSB2007).pdf'), language = 'en', id = 'key1')

#system2("pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf",
 #        stdout = temp.file)

# shell('pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf', mustWork= TRUE)
# test <- system('pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf', 
# intern = TRUE)
# 
# Encoding(test) <- 'UTF-8'
# 
# system(paste0(shQuote("pdfinfo"), shQuote('-enc'), shQuote(encoding), 
#              shQuote(normalizePath('M:/NewPapers/FuSSO(1311.2234v1).pdf'))), intern = TRUE)
# system(shQuote('pdfinfo M:/NewPapers/FuSSO(1311.2234v1).pdf'), intern = TRUE)
# system2('pdfinfo', paste(shQuote('-enc'), shQuote('UTF-8'), 
#                          shQuote(normalizePath('~/NewPapers/FuSSO(1311.2234v1).pdf'))))
# 
# 
# setwd('~/NewPapers')
# system2('pdfinfo', shQuote(normalizePath('M:/NewPapers/FuSSO(1311.2234v1).pdf')))