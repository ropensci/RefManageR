#' Create a BibTeX File from a BibEntry Object
#' 
#' Creates a Bibtex File from a BibEntry object for use with either BibTeX or BibLaTex.
#' @param bib a BibEntry object to be written to file
#' @param file character string naming a file, should; end in \dQuote{.bib}
#' @param biblatex boolean; if \code{TRUE}, \code{\link{toBiblatex}} is used and no conversions of the BibEntry object
#' are done; if \code{FALSE} entries will be converted as described in \code{\link{toBibtex.BibEntry}}.
#' @param append as in \code{\link{write.bib}}
#' @param verbose as in \code{\link{write.bib}}
#' @param ... additional arguments passed to \code{\link{writeLines}}
#' @return \code{bib} - invisibly
#' @author McLean, M. W. - based on \code{write.bib} function in package \code{bibtex} by Francois, R.
#' @seealso \code{\link{write.bib}}, \code{\link{ReadBib}}, \code{\link{toBibtex.BibEntry}}, 
#' \code{\link{toBiblatex}}, \code{\link{BibEntry}}
#' @keywords IO
#' @note To write the contents of \code{bib} \dQuote{as is}, the argument \code{biblatex} should be \code{TRUE}, otherwise
#' conversion is done as in \code{\link{toBibtex.BibEntry}}.
#' @importFrom tools encoded_text_to_latex
#' @author McLean, M. W. based on \code{\link{write.bib}} by Gaujoux, R. in package \code{bibtex}.
#' @export
#' @examples
#' bib <- ReadCrossRef(query = 'carroll journal of the american statistical association', 
#'   year = 2012)
#' tfile <- tempfile(fileext = ".bib")
#' WriteBib(bib, tfile, biblatex = TRUE)
#' identical(ReadBib(tfile), bib)
#' unlink(tfile)
WriteBib <- function (bib, file = "references.bib", biblatex = TRUE, append = FALSE, verbose = TRUE, ...) {
  if (!inherits(bib, "BibEntry"))
    stop("Must supply and object of class BibEntry")
  if (length(bib) == 0) {
    if (verbose) 
      message("Empty bibentry list: nothing to be done.")
    return(invisible())
  }
  if (is.null(file)) 
    file <- stdout()
  else if (is.character(file)) {
    if (!grepl("\\.bib$", file)) 
      file <- paste(file, ".bib", sep = "")
  }
  fh <- file(file, open = if (append) 
    "a+"
             else "w+")
  on.exit(if (isOpen(fh)) close(fh))
  if (verbose) 
    message("Writing ", length(bib), " Bibtex entries ... ", 
            appendLF = FALSE)
  if (biblatex){
    writeLines(toBibtex(bib, ...), fh)  
  }else{
    writeLines(toBiblatex(bib, ...), fh)
  }
  
  if (verbose) 
    message("OK\nResults written to file '", file, "'")
  invisible(bib)
}