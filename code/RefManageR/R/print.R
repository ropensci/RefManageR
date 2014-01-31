#' Print BibLaTeX bibliography Entries
#' 
#' Prints bibliographic information stored in BibEntry objects in BibLaTeX style
#' 
#' @param x - a BibEntry object
#' @param style - an optional character string specifying the print style. If present, 
#' must be a unique abbreviation (with case ignored) of the available styles, see \code{\link{bibentry}}.
#' @param .bibstyle - character string specifying BibLaTeX style to use for formatting references.  Possible values are
#' \dQuote{numeric} (default), \dQuote{authoryear}, \dQuote{authortitle}, \dQuote{alphabetic}, \dQuote{draft}.  See
#' section 3.3.2 of the BibLaTeX manual.
#' @param sorting - how should the entries in \code{x} be sorted?  See \code{\link{sort.BibEntry}}.
#' @param no.print.fields - character vector; fields that should not be printed, e.g., doi, url, isbn, etc.
#' @param max.names - maximum number of names to display for name list fields before truncation with \dQuote{et al.}.
#' @param first.inits - boolean; if true only initials of given names are printed, otherwise full names are used.
#' @param dashed - boolean; for \code{.bibstyle = \dQuote{authoryear}} or \code{.bibstyle = \dQuote{authoryear}} only, 
#' if TRUE duplicate author and editor lists are replaced with \dQuote{---} when printed.
#' @note setting max.names to \code{value} is equivalent to setting \code{maxnames=value} and \code{minnames=value} in BibLaTeX.
#' @references Lehman, Philipp and Kime, Philip and Boruvka, Audrey and Wright, J. (2013). The biblatex Package}.
#' \url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @seealso \code{\link{BibEntry}}, \code{\link{ReadBib}}, \code{\link{format.BibEntry}}, \code{\link{sort.BibEntry}}
#' @examples
#' file.name <- system.file("sampleData", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' print(testb[author="aristotle"], .bibstyle = 'authoryear')
#' print(bib[55:57], .bibstyle = "authortitle", first.inits = FALSE)
#' print(bib[80:88], .bibstyle = "alphabetic", max.names = 1)
#' print(bib[32:36], .bibstyle = "draft")
#' print(bib[editor="westfahl"], .bibstyle = 'authoryear', dashed = TRUE)
print.BibEntry <- function (x, style = "text", .bibstyle = .BibOptions$bib.style, 
                            sorting = .BibOptions$sorting, no.print.fields = NULL, 
                            max.names = .BibOptions$max.names, first.inits = .BibOptions$first.inits, 
                            dashed = .BibOptions$dashed, ...){
  opts <- .BibOptions$copy()
  .BibOptions$max.names <- max.names
  .BibOptions$first.inits <- first.inits
  .BibOptions$dashed <- dashed
  .BibOptions$return.ind <- FALSE
  if (!length(sorting))
    sorting <- switch(.bibstyle, authoryear = 'nyt', alphabetic = 'anyt', draft = 'debug', 'nty')
  style <- .BibEntry_match_format_style(style)
  if (length(x) && length(no.print.fields)){
    for (i in seq_along(no.print.fields))
      x <- do.call(`$<-`, list(x = x, name = tolower(no.print.fields[i]), value = NULL))
  }
  if (style == "R") {
      writeLines(format(x, "R", collapse = TRUE, ...))
  }
  else if (length(x)) {
      y <- format(x, style, .bibstyle, .sorting = sorting, ...)
      if (style == "citation") {
          n <- length(y)
          if (nzchar(header <- y[1L])) 
              header <- c("", header, "")
          if (nzchar(footer <- y[n])) 
              footer <- c("", footer, "")
          writeLines(c(header, paste(y[-c(1L, n)], collapse = "\n\n"), 
              footer))
      }
      else {
          writeLines(paste(y, collapse = "\n\n"))
      }
  }
  .BibOptions <- opts$copy()
  invisible(x)
}