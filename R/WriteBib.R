#' Create a BibTeX File from a BibEntry Object
#'e
#' Creates a Bibtex File from a BibEntry object for use with either BibTeX
#' or BibLaTex.
#' @param bib a BibEntry object to be written to file
#' @param file character string naming a file, should; end in \dQuote{.bib}.
#' Can be \code{NULL}, in which case the BibEntry object will be written
#' to \code{\link{stdout}}.
#' @param biblatex boolean; if \code{TRUE}, \code{\link{toBiblatex}} is used
#' and no conversions of the BibEntry object
#' are done; if \code{FALSE} entries will be converted as described in
#' \code{\link{toBibtex.BibEntry}}.
#' @param append as in \code{write.bib} in package \code{bibtex}
#' @param verbose as in \code{write.bib} in package \code{bibtex}
#' @param ... additional arguments passed to \code{\link{writeLines}}
#' @return \code{bib} - invisibly
#' @seealso \code{write.bib} in package \code{bibtex}, \code{\link{ReadBib}},
#' \code{\link{toBibtex.BibEntry}}, \code{\link{toBiblatex}},
#' \code{\link{BibEntry}}
#' @keywords IO
#' @note To write the contents of \code{bib} \dQuote{as is}, the argument
#' \code{biblatex} should be \code{TRUE}, otherwise
#' conversion is done as in \code{\link{toBibtex.BibEntry}}.
#' @author McLean, M. W. based on \code{write.bib} by Gaujoux, R.
#' in package \code{bibtex}.
#' @export
#' @examples
#' if (requireNamespace("bibtex")){
#'     bib <- BibEntry("Article", key = "Carroll_2012",
#'                     doi = "10.1080/01621459.2012.699793",
#'                     year = "2012", month = "sep",
#'                     volume = 107, number = 499,
#'                     pages = {1166--1177},
#'       author = "R. Carroll and A. Delaigle and P. Hall",
#'       title = "Deconvolution When Classifying Noisy Data ...",
#'       journal = "Journal of the American Statistical Association")
#'
#'   ## Write bib if no server error and bibtex available
#'   if (length(bib)){
#'     tfile <- tempfile(fileext = ".bib")
#'     WriteBib(bib, tfile, biblatex = TRUE)
#'     identical(ReadBib(tfile), bib)
#'     unlink(tfile)
#'   }
#' }
WriteBib <- function (bib, file = "references.bib", biblatex = TRUE,
                      append = FALSE, verbose = TRUE, ...) {
  if (!inherits(bib, "BibEntry"))
    stop("Must supply and object of class BibEntry")
  if (length(bib) == 0) {
    if (verbose)
      message("Empty bibentry list: nothing to be done.")
    return(invisible())
  }
  if (is.null(file))
    fh <- stdout()
  else if (is.character(file)) {
    if (!grepl("\\.bib$", file, useBytes = FALSE))
      file <- paste(file, ".bib", sep = "")
    fh <- file(file, open = if (append)
      "a+"
               else "w+")
    on.exit(if (isOpen(fh)) close(fh))
  }
  if (verbose)
    message("Writing ", length(bib), " Bibtex entries ... ",
            appendLF = FALSE)
  if (biblatex){
    writeLines(toBiblatex(bib, ...), fh)
  }else{
    writeLines(toBibtex(bib, ...), fh)
  }

  if (verbose){
    msg <- paste0("OK\nResults written to ",
            if (is.null(file)) "stdout" else paste0("file ", sQuote(file)))
    message(msg)
  }
  invisible(bib)
}
