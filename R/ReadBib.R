#' BibLaTeX/BibTeX .bib file parser
#'
#' Parser for bibliography databases in the bib format containing either BibLaTeX or BibTeX entries.
#' @param file string; bib file to parse.
#' @param .Encoding encoding
#' @param header header of the citation list. By default this is made from the Preamble entries found in the bib file.
#' @param footer footer of the citation list.
#' @param check \dQuote{error}, \dQuote{warn}, or logical \code{FALSE}.  What action should be taken if an entry is
#' missing required fields?  \code{FALSE} means no checking is done, \dQuote{warn} means entry is added with an error.
#' \dQuote{error} means the entry will not be added.  See \code{\link{BibOptions}}.
#' @author McLean, M. W., based on code in \code{bibtex} package by Francois, R.
#' @importFrom bibtex do_read_bib
#' @importFrom stringr str_trim
#' @seealso \code{\link[bibtex]{read.bib}} in package \code{bibtex}
#' @export
#' @examples
#' file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#' bib <- ReadBib(file.name)
ReadBib <- function (file, .Encoding = "UTF-8",
                     header = if (length(preamble)) paste(preamble, sep = "\n") else "",
                     footer = "", check = BibOptions()$check.entries){
  stopifnot(!missing(file))
  oldchk <- .BibOptions$check.entries
  .BibOptions$check.entries <- check
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(.Encoding, unknown = srcfile(file), srcfile(file,
                                                               encoding = .Encoding))
  ## out <- .External("do_read_bib", file = file, encoding = .Encoding,
  ##                  srcfile = srcfile, PACKAGE = "bibtex")
  out <- do_read_bib(file, encoding = .Encoding, srcfile)
  at <- attributes(out)
  if (typeof(out) != "integer")
    out <- lapply(out, MakeBibEntry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- MakeCitationList(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  .BibOptions$check.entries <- oldchk
  out
}
