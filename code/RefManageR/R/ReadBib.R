#' BibLaTeX/BibTeX .bib file parser
#' 
#' Parser for bibliography databases in the bib format containing either BibLaTeX or BibTeX entries.
#' @param file 
#' @param .Encoding - encoding
#' @param header - header of the citation list. By default this is made from the Preamble entries found in the bib file.
#' @param footer - footer of the citation list.
#' @author McLean, M. W., based on code in \code{bibtex} package by Francois, R. 
#' @imports bibtex 
#' @seealso \code{\link{read.bib}} in package \code{bibtex}
#' @examples 
#' file.name <- system.file("sampleData", "RJC.bib", package="RefManageR")
#' bib <- ReadBib(file.name)
ReadBib <- function (file, .Encoding = "UTF-8", 
                     header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
                     footer = "", check = BibOptions()$check.entries){
  oldchk <- .BibOptions$check.entries
  .BibOptions$check.entries <- check
  stopifnot(!missing(file))
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(.Encoding, unknown = srcfile(file), srcfile(file, 
                                                               encoding = .Encoding))
  out <- .External("do_read_bib", file = file, encoding = .Encoding, 
                   srcfile = srcfile)
  
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, MakeBibEntry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- MakeCitationList(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  .BibOptions$check.entries <- oldchk
  out
}
