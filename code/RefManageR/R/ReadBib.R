#' BibLaTeX/BibTeX .bib file parser
#' 
#' Parser for bibliography databases in the bib format containing either BibLaTeX or BibTeX entries.
#' @param file 
#' @imports bibtex
#' @author Mathew W. McLean \email{mathew.w.mclean@@gmail.com}
ReadBib <- function (file, package = "bibtex", .Encoding = "UTF-8", 
                     header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
                     footer = "") 
{
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
  out
}
