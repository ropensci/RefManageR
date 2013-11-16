# lapply(list.files('M:/biblatex/code/RefManageR/R/', full=TRUE), source)

ReadPDFMeta <- function (file, temp.file = tempfile(), delete.file = TRUE) {
  #outfile <- tempfile("pdfinfo")
  # browser()
  on.exit(unlink(outfile))
  status <- system2("pdfinfo", shQuote(normalizePath(file)), 
                    stdout = outfile)
  tags <- c("Title", "Subject", "Keywords", "Author", "Creator", 
            "Producer", "CreationDate", "ModDate", "Tagged", "Form", 
            "Pages", "Encrypted", "Page size", "File size", "Optimized", 
            "PDF version")
  re <- sprintf("^(%s)", paste(sprintf("%-16s", sprintf("%s:", 
                                                        tags)), collapse = "|"))
  lines <- readLines(outfile, warn = FALSE)
  ind <- grepl(re, lines)
  tags <- sub(": *", "", substring(lines[ind], 1L, 16L))
  info <- split(sub(re, "", lines), cumsum(ind))
  names(info) <- tags
  fmt <- "%a %b %d %X %Y"
  if (!is.null(d <- info$CreationDate)) 
    info$CreationDate <- strptime(d, fmt)
  if (!is.null(d <- info$ModDate)) 
    info$ModDate <- strptime(d, fmt)
  if (!is.null(p <- info$Pages)) 
    info$Pages <- as.integer(p)
  info
}


# meta <- PdfInfo('~/New Papers/BootstrappingClusteredData(JRSSB2007).pdf', 'tempfile')
# 
# reader <- readPDF(PdftotextOptions = "-layout")
# doc <- reader(elem = list(uri = '~/New Papers/BootstrappingClusteredData(JRSSB2007).pdf'), language = 'en', id = 'key1')

