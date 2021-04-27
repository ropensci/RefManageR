#' BibLaTeX/BibTeX .bib file parser
#'
#' Parser for bibliography databases in the bib format containing
#' either BibLaTeX or BibTeX entries.
#' @param file string; bib file to parse.
#' @param .Encoding encoding
#' @param header header of the citation list. By default this is made
#'     from the Preamble entries found in the bib file.
#' @param footer footer of the citation list.
#' @param check \dQuote{error}, \dQuote{warn}, or logical
#'     \code{FALSE}.  What action should be taken if an entry is
#'     missing required fields?  \code{FALSE} means no checking is
#'     done, \dQuote{warn} means entry is added with an error.
#'     \dQuote{error} means the entry will not be added.  See
#'     \code{\link{BibOptions}}.
#' @param use.bibtex Logical, if \code{TRUE} the \code{bibtex} package
#'     will be used to parse the file (if it is available); otherwise,
#'     the \code{python} library \code{bibtextparser} is used.
#' @author McLean, M. W., based on code in \code{bibtex} package by
#'     Francois, R.
#' @importFrom stringr str_trim
#' @note Date fields are parsed using the locale specified by
#'     \code{Sys.getlocale("LC_TIME")}.  To read a bib file with
#'     character \sQuote{month} fields in a language other than the
#'     current locale, \code{Sys.setlocale} should be used to change
#'     \sQuote{LC_TIME}` to match the bib file before calling
#'     \code{ReadBib}.
#'
#' Keys will be made unique by calling \code{\link[base]{make.unique}} with
#' \code{sep = ":"}.
#' @seealso \code{read.bib} in package \code{bibtex}
#' @export
#' @examples
#' if (requireNamespace("bibtex")) {
#'     file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
#'     bib <- ReadBib(file.name)
#' }
ReadBib <- function(file, .Encoding = "UTF-8",
                    header = "", footer = "",
                    check = BibOptions()$check.entries,
                    use.bibtex = FALSE){
    if (use.bibtex && !requireNamespace("bibtex", quietly = TRUE)){
      message("Paramter ", sQuote("use.bibtex"), "is TRUE, but the ",
              dQuote("bibtex"), " package is not installed.\nPlease install from ",
              "GitHub using the ", dQuote("remotes"),
              " (or ", dQuote("devtools"), ") package:\n\n",
              "remotes::install_github(\"ROpenSci/bibtex\").\n")
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

  if (use.bibtex)
    out <- bibtex::do_read_bib(file, encoding = .Encoding, srcfile)
  else
    out <- pyReadBib(srcfile)

  strings <- attr(out, "strings")

  if (typeof(out) != "integer")
    out <- lapply(out, MakeBibEntry, from.bibtex = use.bibtex)
  else out <- list()

  out <- MakeCitationList(out, header, footer)
  out <- MakeKeysUnique(out)

  if (length(strings))
      attr(out, "strings") <- strings

  out
}

#' Parses a bib file using bibtexparser python library
#' @param src.file srcfile object containing bib file
#' @return list containing the parsed bib file entries
#' @noRd
#' @importFrom reticulate import_builtins
pyReadBib <- function(src.file){
  py <- reticulate::import_builtins()
  py$fname <- py$open(path.expand(src.file$filename), "r")
  ## Customizable parser:
  parser <- py.bibtex.parser$bparser$BibTexParser()
  parser$ignore_nonstandard_types <- FALSE
  py.bib.db <- parser$parse_file(py$fname)
  py.dict <- py.bib.db$entries
  if (length(py.bib.db$strings))
      attr(py.dict, "strings") <- py.bib.db$strings
  ## with(py$open(file, "w") %as% fname, {
  ##     py$bib.db <- py.bibtex.parser$load(fname)
  ## })
  ## py$bib_str <- paste0(readLines(file), collapse = "\n")
  ## py.bib.db <- py.bibtex.parser$loads(py$bib_str)
  return(py.dict)
}
