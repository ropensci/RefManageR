# lapply(list.files('M:/biblatex/code/RefManageR/R/', full=TRUE), source)

ReadPDFMeta <- function (path, temp.file = tempfile(), delete.file = TRUE, encoding = 'UTF-8', recursive = TRUE) {
  #outfile <- tempfile("pdfinfo")
   browser()
#   if (delete.file)
#     on.exit(unlink(temp.file))
  
  files <- list.files(path, pattern = '*.pdf', full.name = TRUE, recursive = recursive)
  if (!length(files))  # check if directory or file specified
    files <- path
  
#  out <- lapply(files, function(x) system(paste(shQuote("pdfinfo"), shQuote('-enc'), shQuote(encoding), 
#                                                              shQuote(normalizePath(x))), intern = TRUE)) 
  out <- lapply(files, function(x) system2("pdfinfo", paste(shQuote('-enc'), shQuote(encoding), 
                                                 shQuote(normalizePath(x))), stdout = TRUE, stderr = TRUE))
   
   res <- lapply(out, ProcessPDFMeta, encoding=encoding)
   res <- lapply(res, MakeBibEntry, GS = TRUE)
   res <- MakeCitationList(res)
   res
}

ProcessPDFMeta <- function(x, encoding = 'UTF-8', path){
  #status <- system2("pdfinfo", args = c('-enc', encoding, shQuote(normalizePath(file))), 
  #                  stdout = outfile)
  tags <- c("Title", "Subject", "Author")
#             , "Creator", 
#             "Producer", "CreationDate", "ModDate", "Tagged", "Form", 
#             "Pages", "Encrypted", "Page size", "File size", "Optimized", 
#             "PDF version")
  re <- sprintf("^(%s)", paste(sprintf("%-16s", sprintf("%s:", 
                                                        tags)), collapse = "|"))
  #lines <- readLines(temp.file, warn = FALSE)
  # ind <- which(grepl(re, x))
  ind <- pmatch(tags, substring(x, 1L, 16L))
  if (any(is.na(ind)) || any((info <- sub(re, "", x[ind]))=='')){
    message('Missing Title, Author, or Subject metadata, the following entry will be ignored')
    message(path)
    return(NA)
  }
  
  #tags <- sub(": *", "", substring(x, 1L, 16L))
  #info <- split(sub(re, "", x), cumsum(ind))
  #info <- sub(re, "", x)[ind]
  names(info) <- tags[ind]
  Encoding(info) <- encoding
  res <- list(title = info['Title'],
              author = as.personList(info['Author']))
  
  # add keywords if available
  ind <- pmatch('Keywords', substring(x, 1L, 16L))
  if(!is.na(ind))
    res$keywords <- sub('Keywords:[[:space:]]+', '', x[ind])
  
  subj.res <- ProcessPDFSubject(info['Subject'])
  
  if (is.null(subj.res$year)){
    ind <- pmatch('CreationDate', substring(x, 1L, 16L))
    if(!is.na(ind)){
      res$date <- sub('CreationDate:[[:space:]]+', '', x[ind])
      res$date <- strptime(res$year, format = "%m/%d/%y %H:%M:%S")
    }else{
      message('Cannot determine year/date, the following entry will be ignored')
      message(path)
      return(NA)      
    }
  }
  
  # create key
  aut <- tolower(strsplit(info['Author'], ' ')[[1]][2])  # get last name of first author
  aut <- gsub(',', '', aut)  # remove trailing commas
  first.word <- tolower(strsplit(res$title, ' ')[[1]][[1]])  # get first word of title
  attr(res, 'key') <- paste0(aut, res$year, first.word)
  
  attr(res, 'entry') <- 'article'
  
  return(res)
#   fmt <- "%a %b %d %X %Y"
#   if (!is.null(d <- info$CreationDate)) 
#     info$CreationDate <- strptime(d, fmt)
#   if (!is.null(d <- info$ModDate)) 
#     info$ModDate <- strptime(d, fmt)
#   if (!is.null(p <- info$Pages)) 
#     info$Pages <- as.integer(p)
#   info
}


ProcessPDFSubject(subj){
  if(subj == '')
    return(list())
  strs <- strsplit(subj, ',')
  m <- regexpr('/^[^,]*/', subj, perl = TRUE)
  m <- regexpr('[[:print:^,]]+,', subj)
  if(m != -1){
    journal <- regmatches(subj, m)  
  }else{
    return(list(notes))
  }
}

 meta <- ReadPDFMeta('~/NewPapers/')
# 
# reader <- readPDF(PdftotextOptions = "-layout")
# doc <- reader(elem = list(uri = '~/New Papers/BootstrappingClusteredData(JRSSB2007).pdf'), language = 'en', id = 'key1')

#system2("pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf",
 #        stdout = temp.file)

shell('pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf', mustWork= TRUE)
test <- system('pdfinfo M:/NewPapers/BootstrappingClusteredData(JRSSB2007).pdf', 
intern = TRUE)

Encoding(test) <- 'UTF-8'

system(paste0(shQuote("pdfinfo"), shQuote('-enc'), shQuote(encoding), 
             shQuote(normalizePath('M:/NewPapers/FuSSO(1311.2234v1).pdf'))), intern = TRUE)
system(shQuote('pdfinfo M:/NewPapers/FuSSO(1311.2234v1).pdf'), intern = TRUE)
system2('pdfinfo', paste(shQuote('-enc'), shQuote('UTF-8'), 
                         shQuote(normalizePath('~/NewPapers/FuSSO(1311.2234v1).pdf'))))


setwd('~/NewPapers')
system2('pdfinfo', shQuote(normalizePath('M:/NewPapers/FuSSO(1311.2234v1).pdf')))