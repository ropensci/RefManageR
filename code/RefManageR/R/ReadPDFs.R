# lapply(list.files('M:/biblatex/code/RefManageR/R/', full=TRUE), source)

ReadPDFs <- function (path, encoding = 'UTF-8', recursive = TRUE, use.crossref = TRUE, use.metadata = TRUE) {
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
  
  #########################################################################
  # read metadata and search for DOI
  ########################################################################
  if (use.metadata){
    message(paste0('Getting Metadata for ',length(files), ' pdfs...'))
    flush.console()
    out <- lapply(files, function(x) system2("pdfinfo", paste(shQuote('-enc'), shQuote(encoding), 
                                              shQuote(normalizePath(x))), stdout = TRUE, stderr = TRUE))
    
    dois <- sapply(out, SearchDOIText)
    not.done <- which(is.na(dois))
  }

  ########################################
  # search first two pages of pdf for DOI
  ########################################
#   tdir <- tempdir()
#   on.exit(unlink(tdir))
 # browser()
  #SearchDOIFirstPage <- function(path, enc = encoding){
  txt.files <- vector('list', length(not.done))
  more.dois <- vector('character', length(not.done))
  for (i in seq.int(n.files)){
    tpath <- files[i]
    system2("pdftotext", paste(shQuote('-l'), shQuote('2'), shQuote('-enc'), shQuote(encoding), 
                               shQuote(normalizePath(tpath))))
    tmp.file <- sub('.pdf', '.txt', tpath)
    txt.files[[i]] <- suppressWarnings(readLines(tmp.file, encoding = encoding))
    if (!use.metadata || i %in% not.done)  # if didn't find doi in meta search first two pages
      more.dois[i] <- SearchDOIText(txt.files[[i]])
    file.remove(tmp.file)
  }
  #more.dois <- sapply(files[not.done], SearchDOIFirstPage, enc = encoding)
  dois[not.done] <- more.dois[not.done]

  res <- NULL
  if (use.crossref){
    #not.done <- not.done[is.na(more.dois)]
    not.na1 <- !is.na(dois)
   # dois <- c(dois[-not.done], na.omit(more.dois))
    message(paste0('Getting ', sum(not.na1), ' BibTeX entries from CrossRef...'))
    flush.console()
    # res <- lapply(dois, ReadCrossRef)
    res <- llply(as.list(dois[not.na1]), ReadCrossRef, .progress = progress_text(char = "."))
    message('Done')
    not.done <- seq.int(n.files)[-which(not.na1)[!is.na(res)]]
  }

  if (length(not.done)){
    message(paste0('Attempting to create BibTeX entries from first PDF pages for ', length(not.done), ' entries...'))
    res2 <- mapply(ReadFirstPages, txt.files[not.done], files[not.done], SIMPLIFY = FALSE)  # lapply(out, ProcessPDFMeta, encoding=encoding)
    message('Done.')
  #  files2 <- files[!na2]
    flush.console()
  #  not.done <- not.done[na2]
  }
 # browser()
  res3 <- NULL
  if (use.metadata && length(not.done)){
   message(paste0('Attempting to create BibTeX entries from PDF metadata for ', length(not.done), ' entries...'))
   res3 <- mapply(ProcessPDFMeta, out[not.done], files[not.done], MoreArgs = list(enc=encoding, check.doi = !use.crossref), 
                 SIMPLIFY = FALSE)  # lapply(out, ProcessPDFMeta, encoding=encoding)
   message('Done.')
   flush.console()
  # na3 <- is.na(res3)
   #res3 <- res3[!na3]
   res2 <- AddListToList(res2, res3)
  # files3 <- files[!na3]
   not.done <- not.done[is.na(res2)]
   
   #res2 <- lapply(res2, MakeBibEntry, GS = TRUE)
   #res2 <- MakeCitationList(res2)
   #res2 <- c(res2, res3)
  }#else{
  #  not.done <- not.done[na2]
  #  not.done <- not.done[na2]
  #}
  res2 <- res2[!is.na(res2)]
  res2 <- lapply(res2, MakeBibEntry, GS = TRUE)
  res <- c(res, res2)

  # add file names and dois as fields (if doi not done already)
  if (length(not.done) < length(out)){
    inds <- seq.int(n.files)[-not.done]
    for (i in 1L:length(inds)){
      res[[i]]$file <- files[inds[i]]
      if (!use.crossref && !is.na(dois[inds[i]])) 
        res[[i]]$doi <- dois[inds[i]]
    }
  }
  
  res <- MakeCitationList(res)
  
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