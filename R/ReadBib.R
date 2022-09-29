#' BibLaTeX/BibTeX .bib file parser
#'
#' Parser for bibliography databases in the bib format containing either BibLaTeX or
#' BibTeX entries.
#' @param file string; bib file to parse.
#' @param .Encoding encoding
#' @param header header of the citation list. By default this is made from the Preamble
#' entries found in the bib file.
#' @param footer footer of the citation list.
#' @param check \dQuote{error}, \dQuote{warn}, or logical \code{FALSE}.  What action
#' should be taken if an entry is missing required fields?  \code{FALSE} means no
#' checking is done, \dQuote{warn} means entry is added with an error.
#' \dQuote{error} means the entry will not be added.  See \code{\link{BibOptions}}.
#' @author McLean, M. W., based on code in \code{bibtex} package by Francois, R.
#' @importFrom stringr str_trim
#' @importFrom bibtex do_read_bib
#' @note Date fields are parsed using the locale specified by
#' \code{Sys.getlocale("LC_TIME")}.  To read a bib file with character \sQuote{month}
#' fields in a language other than the current locale, \code{Sys.setlocale} should be
#' used to change \sQuote{LC_TIME}` to match the bib file before calling \code{ReadBib}.
#'
#' Keys will be made unique by calling \code{\link[base]{make.unique}} with
#' \code{sep = ":"}.
#' @seealso \code{\link[bibtex]{read.bib}} in package \code{bibtex}
#' @export
#' @examples
#' if (requireNamespace("bibtex")) {
#'     file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#'     bib <- ReadBib(file.name)
#' }
ReadBib <- function(file, .Encoding = "UTF-8",
                    header = if (length(preamble)) paste(preamble,
                                                         sep = "\n") else "",
                    footer = "", check = BibOptions()$check.entries){
    if (!requireNamespace("bibtex")){
      message("Sorry this feature currently cannot be used without the ",
              dQuote("bibtex"), " package installed.\nPlease install from ",
              "GitHub using the ", dQuote("remotes"),
              " (or ", dQuote("devtools"), ") package:\n\n",
              "remotes::install_github(\"ropensci/bibtex\")")
      return(invisible())
    }

  stopifnot(!missing(file))
  old.chk <- BibOptions(check.entries = check)
  on.exit(BibOptions(old.chk))

  if (!is.character(file)) {
      stop(gettextf("%s only supports reading from files, %s should be %s",
                    sQuote("ReadBib"), sQuote("file"),
                    "a character vector of length one"))
  }
  srcfile <- switch(.Encoding, unknown = srcfile(file),
                    srcfile(file, encoding = .Encoding))

  out <- bibtex::do_read_bib(file, encoding = .Encoding, srcfile)
  at <- attributes(out)
  if (typeof(out) != "integer")
    out <- lapply(out, MakeBibEntry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- MakeCitationList(out, header, footer)
  out <- MakeKeysUnique(out)

  strings <- at[["strings"]]
  if (length(strings))
      attr(out, "strings") <- strings

  out
}
