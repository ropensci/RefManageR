open.BibEntry <- function(dtb, entry = 1, open.field = c('file', 'url', 'eprint', 'doi'), viewer){
  bib <- dtb[[entry]]
  stopifnot(length(dtb[[entry]]) == 1)
  bib.fields <- unlist(fields(bib))
  opened <- FALSE
  i <- 1
  if (missing(viewer))
    viewer <- getOption('browser')
  
  while (!opened && i <= length(open.field)){
    if (open.field[i]=='file' && 'file' %in% bib.fields){
      if (missing(viewer))
        viewer <- getOption('pdfviewer')
      browseURL(paste0('file://', bib['file']), viewer) 
      opened <- TRUE
    }else if (open.field[i]=='eprint' && 'eprint' %in% bib.fields){
      if (is.null(viewer))
        viewer <- getOption('browser')
      eprinttype <- suppressMessages(tolower(bib['eprinttype']))
      if (length(eprinttype)){
        base.url <- switch(eprinttype, jstor = 'http://www.jstor.org/stable/',
                           arxiv = 'http://arxiv.org/abs/', 
                           pubmed = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&cmd=prlinks&retmode=ref&id=')
        if (!is.null(base.url))
          browseURL(paste0(base.url, bib['eprint']), viewer)
      }
      opened <- TRUE
    }else if (open.field[i]=='doi' && 'doi' %in% bib.fields){
      browseURL(paste0('http://dx.doi.org/', bib['doi']), viewer)
      opened <- TRUE
    }else if (open.field[i]=='url' && 'url' %in% bib.fields){
      browseURL(bib['url'], viewer)
      opened <- TRUE
    }
    i <- i + 1
  }
  if (!opened)
    message('Could not open the specified entry.')
}