#' Print BibLaTeX bibliography Entries
#'
#' Prints bibliographic information stored in BibEntry objects in BibLaTeX style
#'
#' @param x a BibEntry object
#' @param .opts a list of formatting options from \code{\link{BibOptions}}.  Possible options are
#' \itemize{
#' \item \code{style} - character string naming the printing style.  Possible values are
#' plain text (style \dQuote{text}), BibTeX (\dQuote{Bibtex}), BibLaTeX
#' (\dQuote{Biblatex}), a mixture of plain text and BibTeX as
#' traditionally used for citations (\dQuote{citation}), HTML (\dQuote{html}),
#' LaTeX (\dQuote{latex}), \dQuote{markdown},
#' R code (\dQuote{R}), and a simple copy of the textVersion elements
#' (style \dQuote{textVersion}, see \code{\link{BibEntry}})
#' \item \code{bib.style} - character string specifying BibLaTeX style to use for formatting
#' references.  Possible values are \dQuote{numeric} (default), \dQuote{authoryear},
#' \dQuote{authortitle}, \dQuote{alphabetic}, \dQuote{draft}.  See section 3.3.2 of the
#' BibLaTeX manual.
#' \item \code{sorting} - how should the entries in \code{x} be sorted?  See
#' \code{\link{sort.BibEntry}}.
#' \item \code{max.names} - maximum number of names to display for name list fields before
#' truncation with \dQuote{et al.}.
#' \item \code{first.inits} - logical; if true only initials of given names are printed,
#' otherwise full names are used.
#' \item \code{dashed} - logical; for \code{.bibstyle = "authoryear"} or
#' \code{.bibstyle = "authoryear"} only,
#' if \code{TRUE} duplicate author and editor lists are replaced with \dQuote{---} when printed.
#' \item \code{no.print.fields} character vector; fields that should not be printed,
#' e.g., doi, url, isbn, etc.
#' }
#' @param ... extra parameters to pass to the renderer.
#' @method print BibEntry
#' @export
#' @importFrom tools toRd
#' @note setting max.names to \code{value} is equivalent to setting \code{maxnames=value} and
#' \code{minnames=value} in BibLaTeX.
#'
#' Custom BibLaTeX styles may be defined using the function \code{\link{bibstyle}}.  To fully
#' support BibLaTeX, the created
#' environment must have functions for formatting each of the entry types decribed in
#' \code{\link{BibEntry}}.
#' @references Lehman, Philipp and Kime, Philip and Boruvka, Audrey and Wright, J. (2013). The biblatex
#' Package. \url{http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
#' @seealso \code{\link{BibEntry}}, \code{\link{ReadBib}}, \code{\link{sort.BibEntry}}
#' @examples
#' file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#' bib <- suppressMessages(ReadBib(file.name))
#' print(bib[author="aristotle"], .opts = list(bib.style = "numeric"))
#' print(bib[55:57], .opts = list(bib.style = "authortitle", first.inits = FALSE))
#' print(bib[80:88], .opts = list(bib.style = "alphabetic", max.names = 1, no.print.fields = "issn"))
#' print(bib[32:36], .opts = list(bib.style = "draft"))
#' oldopts <- BibOptions(bib.style = "authoryear", dashed = TRUE, sorting = "ydnt")
#' bib[editor = "westfahl"]
#' BibOptions(oldopts)
print.BibEntry <- function (x, .opts = list(), ...){
  if (length(.opts)){
    oldopts <- BibOptions(.opts)
    on.exit(BibOptions(oldopts))
  }
  style <- .BibOptions$style

  sorting <- if (!length(.BibOptions$sorting))
    sorting <- switch(.BibOptions$bib.style, authoryear = 'nyt', alphabetic = 'anyt', draft = 'debug', 'nty')
  else .BibOptions$sorting

  style <- .BibEntry_match_format_style(style)
  no.print <- .BibOptions$no.print.fields
  if (length(x) && length(no.print)){
    for (i in seq_along(no.print))
      x <- do.call(`$<-`, list(x = x, name = tolower(no.print[i]), value = NULL))
  }
  if (style == "R") {
    writeLines(format(x, style, collapse = TRUE, ...))
  }else if (length(x)) {
      y <- format(x, style = style, .BibOptions$bib.style, .sorting = sorting,
                  .sort = TRUE, ...)
      if (style == "citation") {
          n <- length(y)
          if (nzchar(header <- y[1L]))
              header <- c("", header, "")
          if (nzchar(footer <- y[n]))
              footer <- c("", footer, "")
          writeLines(c(header, paste(y[-c(1L, n)], collapse = "\n\n"),
              footer))
      }else {
          writeLines(paste(y, collapse = "\n\n"))
      }
  }
  invisible(x)
}
