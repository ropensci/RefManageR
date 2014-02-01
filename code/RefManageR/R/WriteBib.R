#' Create a BibTeX File from a BibEntry Object
#' 
#' Creates a Bibtex File from a BibEntry object for use with either BibTeX or BibLaTex.
#' @param bib - a BibEntry object to be written to file
#' @param file - character string naming a file, should; end in \dQuote{.bib}
#' @param biblatex - boolean; if \code{TRUE}, \code{\link{toBiblatex}} is used and no conversions of the BibEntry object
#' are done; if \code{FALSE} entries will be converted as described in \code{\link{toBibtex.BibEntry}}.
#' @param append - as in \code{\link{write.bib}}
#' @param verbose - as in \code{\link{write.bib}}
#' @author McLean, M. W. - based on \code{write.bib} function in package \code{bibtex} by Francois, R.
#' @seealso \code{\link{write.bib}}, \code{\link{toBibtex.BibEntry}}, \code{\link{toBiblatex}}, \code{\link{BibEntry}}
#' @keywords IO
WriteBib <- function (bib, file = "references.bib", biblatex = TRUE, append = FALSE, verbose = TRUE, ...) {
  bibs <- if (inherits(bib, "bibentry")) 
    bib
  else if (is.character(bib)) {
    if (length(bib) == 0) {
      if (verbose) 
        message("Empty package list: nothing to be done.")
      return(invisible())
    }
    pkgs <- bib
    if (is.null(pkgs)) 
      pkgs <- unique(installed.packages()[, 1])
    bibs <- sapply(pkgs, function(x) try(citation(x)), simplify = FALSE)
    n.installed <- length(bibs)
    ok <- sapply(bibs, inherits, "bibentry")
    pkgs <- pkgs[ok]
    bibs <- bibs[ok]
    n.converted <- sum(ok)
    pkgs <- lapply(seq_along(pkgs), function(i) if (length(bibs[[i]]) > 
                                                      1) 
      paste(pkgs[i], 1:length(bibs[[i]]), sep = "")
                   else pkgs[i])
    pkgs <- do.call("c", pkgs)
    bibs <- do.call("c", bibs)
    as.bibkey <- function(x) {
      i <- grep("[.]", x)
      if (length(i) > 0) 
        x[i] <- paste("{", x[i], "}", sep = "")
      x
    }
    bibs <- mapply(function(b, k) {
      b$key <- k
      b
    }, bibs, pkgs, SIMPLIFY = FALSE)
    bibs <- do.call("c", bibs)
    if (verbose) 
      message("Converted ", n.converted, " of ", n.installed, 
              " package citations to BibTeX")
    bibs
  }
  else stop("Invalid argument 'entry': expected a bibentry object or a character vector of package names.")
  if (length(bibs) == 0) {
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
    message("Writing ", length(bibs), " Bibtex entries ... ", 
            appendLF = FALSE)
  if (biblatex){
    writeLines(toBibtex(bibs, ...), fh)  
  }else{
    writeLines(toBiblatex(bibs, ...), fh)
  }
  
  if (verbose) 
    message("OK\nResults written to file '", file, "'")
  invisible(bibs)
}