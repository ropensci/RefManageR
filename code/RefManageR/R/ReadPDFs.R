# lapply(list.files('M:/biblatex/code/RefManageR/R/', full=TRUE), source)

ReadPDFs <- function (path, encoding = 'UTF-8', recursive = TRUE, use.crossref = TRUE) {
  #outfile <- tempfile("pdfinfo")
  # browser()
#   if (delete.file)
#     on.exit(unlink(temp.file))
  
  files <- list.files(path, pattern = '.pdf$', full.name = TRUE, recursive = recursive)
  if (!length(files))  # check if directory or file specified
    files <- path
  
#  out <- lapply(files, function(x) system(paste(shQuote("pdfinfo"), shQuote('-enc'), shQuote(encoding), 
#                                                              shQuote(normalizePath(x))), intern = TRUE)) 
  message(paste0('Running Poppler pdfinfo on ',length(files), ' pdfs...'))
  flush.console()
  n.files <- shQuote(normalizePath(files))
  out <- lapply(n.files, function(x) system2("pdfinfo", paste(shQuote('-enc'), shQuote(encoding), 
                                                 x), stdout = TRUE, stderr = TRUE))
#  message(paste0('Constructing bibliography entries')) 
#  flush.console()
  not.done <- seq_along(out)
  if (use.crossref){
    dois <- sapply(out, SearchDOIMeta)
    not.done <- which(is.na(dois))
    
    more.dois <- sapply(files[not.done], SearchDOIFirstPage, enc = encoding)
    dois <- c(dois[-not.done], na.omit(more.dois))
    not.done <- not.done[is.na(more.dois)]
    message(paste0('Getting ', length(dois), ' BibTeX entries from CrossRef...'))
    flush.console()
    res <- lapply(dois, ReadCrossRef)
    if (length(not.done) < length(out)){
      res <- MakeCitationList(res)
      inds <- seq_along(out)[-not.done]
      for (i in 1:length(inds))
           res[[i]]$file <- files[inds[i]]
    }
  }
  browser()
   message(paste0('Attempting to create BibTeX entries from PDF metadata for ', length(not.done), ' entries...'))
   res2 <- mapply(ProcessPDFMeta, out[not.done], files[not.done], MoreArgs = list(enc=encoding, check.doi = !use.crossref), 
                 SIMPLIFY = FALSE)  # lapply(out, ProcessPDFMeta, encoding=encoding)
   flush.console()
   res2 <- res2[!is.na(res2)]
   res2 <- lapply(res2, MakeBibEntry, GS = TRUE)
   res2 <- MakeCitationList(res2)
   c(res, res2)
}

SearchDOIMeta <- function(meta){
  pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
  m <- regexpr(pattern, meta, perl=TRUE)
  if(all(m == -1)){
    return(NA)
  }else{
    return(regmatches(meta, m)[1])
  }
}

SearchDOIFirstPage <- function(path, enc = encoding){
  #browser()
  system2("pdftotext", paste(shQuote('-l'), shQuote('1'), shQuote('-enc'), shQuote(enc), shQuote(normalizePath(path))))
  tmp.file <- sub('.pdf', '.txt', path)
  txt <- suppressWarnings(readLines(tmp.file))
  res <- SearchDOIMeta(txt)
  file.remove(tmp.file)
  return(res)
}

ProcessPDFMeta <- function(x, enc = 'UTF-8', path, check.doi = FALSE){
  #status <- system2("pdfinfo", args = c('-enc', encoding, shQuote(normalizePath(file))), 
  #                  stdout = outfile)
 # browser()
  Encoding(x) <- enc
  tags <- c("Title", "Author")
#             , "Creator", "Subject"
#             "Producer", "CreationDate", "ModDate", "Tagged", "Form", 
#             "Pages", "Encrypted", "Page size", "File size", "Optimized", 
#             "PDF version")
  re <- sprintf("^(%s)", paste(sprintf("%-16s", sprintf("%s:", 
                                                        tags)), collapse = "|"))
  #lines <- readLines(temp.file, warn = FALSE)
  # ind <- which(grepl(re, x))
  found.tags <- substring(x, 1L, 16L)
  ind <- pmatch(tags, found.tags)
  if (any(is.na(ind)) || any((info <- sub(re, "", x[ind]))=='')){
    message('Ignoring the following entry due to missing Title and/or Author metadata:')
    message(path)
    return(NA)
  }
  
  #tags <- sub(": *", "", substring(x, 1L, 16L))
  #info <- split(sub(re, "", x), cumsum(ind))
  #info <- sub(re, "", x)[ind]
  res <- list(title = info[1],
              author = as.person(info[2]))
  
  # add keywords if available
  ind <- pmatch('Keywords', found.tags)
  if(!is.na(ind))
    res$keywords <- sub('Keywords:[[:space:]]+', '', x[ind])
  
  ind <- pmatch('Subject', found.tags)
  if (!is.na(ind)){
    subj <- sub('Subject:[[:space:]]+', '', x[ind])
    
    if (subj != ''){
      res <- c(res, ProcessPDFSubject(subj, check.doi))
    }
  }
  

  
  # if year not in Subject, use ModDate or CreationDate
  if (is.null(res$year)){
    ind <- pmatch('ModDate', found.tags)
    if(!is.na(ind)){
      res$date <- sub('ModDate:[[:space:]]+', '', x[ind])
      res$date <- strptime(res$date, format = "%m/%d/%y %H:%M:%S")
      if (!inherits(res$date, 'try-error')){
        res$year <- year(res$date)
        res$date <- trunc(res$date, 'days')
      }
    }else if (!is.na(ind <-pmatch('CreationDate', found.tags))){
      res$date <- sub('CreationDate:[[:space:]]+', '', x[ind])
      res$date <- try(strptime(res$date, format = "%m/%d/%y %H:%M:%S"), TRUE)
      if (!inherits(res$date, 'try-error')){
        res$year <- year(res$date)
        res$date <- trunc(res$date, 'days')
      }
    }
    # if year still NULL due to no ModDate or CreationDate, or wrong format of either: give up
    if (is.null(res$year)){
      message('Cannot determine year/date, the following entry will be ignored:')
      message(path)
      return(NA)      
    }
  }
  
  res$file <- path
  
  # create key
  aut <- tolower(res$author[1]$family[1])  # get last name of first author
  aut <- gsub(',', '', aut)  # remove trailing commas
  first.word <- tolower(strsplit(res$title, ' ')[[1]][[1]])  # get first word of title
  attr(res, 'key') <- paste0(aut, res$year, first.word)
  
  if (!is.null(res$journal)){
    attr(res, 'entry') <- 'article'
  }else{
    attr(res, 'entry') <- 'misc'
  }
  
  return(res)
}


ProcessPDFSubject <- function(subj, check.doi = FALSE){
  res <- list()
  
  # first check for doi
  if (check.doi){
    pattern  <- "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
    m <- regexpr(pattern, temp, perl=TRUE)
    if (m != -1){
      res$doi <- regmatches(temp, m)
      subj <- regmatches(temp, m, invert = TRUE)
      subj <- gsub('doi:', '', subj)
    }
  }
  
  strs <- strsplit(subj, ',[[:space:]]?')[[1]]
  # m <- regexpr('/^[^,]*/', subj, perl = TRUE)
  # m <- regexpr('[[:print:^,]]+,', subj)
  res$journal <- strs[1]
  strs <- strs[-1]
  
  m <- regexpr('^[Vv]ol.[[:space:]]?', strs)
  ind <- which(m!=-1)
  if (length(ind)){
    res$volume <- gsub('^[Vv]ol.[[:space:]]?', '', strs[ind])
    strs <- strs[-ind]
  }

  m <- regexpr('^[Nn]o.[[:space:]]?', strs)
  ind <- which(m!=-1)
  if (length(ind)){
    res$number <- gsub('^[No]o.[[:space:]]?', '', strs[ind])
    strs <- strs[-ind]
  }
  
  m <- regexpr('^[[:digit:]]{4}$', strs)
  ind <- which(m!=-1)
  if (length(ind)){
    res$year <- regmatches(strs, m)
    strs <- strs[-ind]
  }
  
  # be extra careful matching hypen. usuallly it's \u2013, an en dash
  m <- regexpr('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', strs)
  ind <- which(m!=-1)
  if (length(ind)){
    res$pages <- grep('[0-9]+[-\u2212\ufe58\ufe63\uff0d\u2012-\u2015][0-9]+', strs[ind], value=TRUE)
    strs <- strs[-ind]
  }
  
  # if anything is left in the subject, add it to note field
  if(length(strs))
    res$note <- paste(strs, collapse = '')
  
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