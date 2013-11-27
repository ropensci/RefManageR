# lapply(list.files('M:/biblatex/code/RefManageR/R/', full=TRUE), source)
library(plyr)
ReadPDFs <- function (path, .enc = 'UTF-8', recursive = TRUE, use.crossref = TRUE, use.metadata = TRUE) {
  #outfile <- tempfile("pdfinfo")
  # browser()
#   if (delete.file)
#     on.exit(unlink(temp.file))
#  files1 <- files2 <- files3 <- NULL
  
  files <- list.files(path, pattern = '.pdf$', full.name = TRUE, recursive = recursive)
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
  resJSTOR <- mapply(CheckJSTOR, txt.files1, txt.files2)
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
  resCR <- NULL
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
    CR.ind <- !is.na(resCR)      # on very rare instances CrossRef doesn't have record for particular DOI
    badCR.ind <- doi.ind[!CR.ind]
    if(any(CR.ind)){
#       for (i in which(CR.ind))
#         resCR[[i]]$file <- files[doi.ind[i]]
      not.done <- not.done[!not.done %in% doi.ind[CR.ind]]
    }
    
    resCR <- MakeCitationList(resCR[CR.ind])  # llply returns a list of BibEntry objs, NOT single BibEntry obj
   # class(res) <- c('Bibentry', 'bibentry')
   #  not.done <- not.done[-doi.ind[CR.ind]] # seq.int(n.files)[-which(not.na1)[!is.na(res)]]
  }
  
  # get bib info from first page. if dont find abstract on first page, use second page too
  res <- lapply(txt.files1[not.done], ReadFirstPages)
  # not.done <- not.done[is.na(res) || !res$found.abstract]
  res2 <- lapply(txt.files2[not.done], ReadFirstPages)
  

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
   res <- mapply(CleanAuthorTitle, res, res2, resMeta, files[not.done])
  }else{
   res <- mapply(CleanAuthorTitle, res, res2, files[not.done], MoreArgs = list(resMeta=NULL)) 
  }
  #               JSTOR        ReadPDF+Meta           CrossRef
  done.inds <- c(done.inds, not.done[!is.na(res)], doi.ind[CR.ind])
  not.done <- not.done[is.na(res)]
  res <- res[!is.na(res)]  # remove entries with no author or title info - done in MakeCitationList

  res <- c(resJSTOR, res)
  res <- lapply(res, MakeBibEntry, GS = TRUE)
  res <- MakeCitationList(res)
  
  if (use.crossref)
    res <- c(res, resCR)

  
  # add file names and dois as fields (if doi not done already)
  if (length(not.done) < length(out)){
    res$files <- files[done.inds]
   # browser()
    if (!use.crossref && length(doi.ind)){
      res[done.inds %in% doi.ind]$doi <- comb.doi[doi.ind %in% done.inds]
    }else if (use.crossref && length(badCR.ind)){  # DOI's missed by CrossRef may be fixed up later
      res[done.inds %in% badCR.ind]$doi <- comb.doi[doi.ind %in% badCR.ind]
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