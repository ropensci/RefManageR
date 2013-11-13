GetBibEntryWithDOI <- function(doi, temp.file='tempfile.bib', delete.file=TRUE){
  file.create(temp.file)
  for(i in 1:length(doi)){
    temp <- getURLContent(url=paste0('http://dx.doi.org/', doi),
                          .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"), followLocation=TRUE))
    if(is.raw(temp))
      temp <- rawToChar(temp)
    write(temp, file = temp.file, append=TRUE)
  }
  bib.res <- read.bib(file=temp.file, encoding='UTF-8')
  
  if(delete.file)
    suppressWarnings(file.remove(temp.file))
  bib.res
}