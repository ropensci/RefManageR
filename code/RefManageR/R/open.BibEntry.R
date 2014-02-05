#' Open BibEntry in PDF viewer or web browser.
#' 
#' @param bib object of class BibEntry.
#' @param entry numeric index or character key of entry in \code{bib} to open.
#' @param open.field character vector of fields to use in \code{bib} to open the BibEntry.  
#'   Possible fields are any combination of \dQuote{file},\dQuote{url}, \dQuote{eprint}, or \dQuote{doi}.  
#'   \dQuote{eprint} is implemented for \code{eprinttype=} \dQuote{JSTOR}, \dQuote{PubMed}, or \dQuote{arXiv}.  
#'   When multiple fields are specified, they are tried in the order they appear in the vector.
#' @param viewer character string giving the name of the program to be used as hypertext browser. 
#'   It should be in the PATH, or a full path specified. Alternatively, an R function to be called to invoke 
#'   the browser.  Defaults to \code{getOptions(\dQuote{pdfviewer})} if \code{open.field = \dQuote{file}} and 
#'   \code{getOptions(\dQuote{browser})}, otherwise.
#' @keywords connection utilities  
#' @seealso \code{\link{browseURL}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @method open BibEntry
#' @examples 
#' \dontrun{
#' testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex"))
#' open(testbib)
#' testbib$file <- file.path(paste0(R.home("doc"), '/manual/R-intro.pdf'))
#' open(testbib)
#' }
open.BibEntry <- function(bib, entry = 1, open.field = c('file', 'url', 'eprint', 'doi'), viewer){
  bib <- bib[[entry]]
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