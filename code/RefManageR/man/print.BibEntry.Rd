\name{print.BibEntry}
\alias{print.BibEntry}
\title{Print BibLaTeX bibliography Entries}
\usage{
\method{print}{BibEntry}(x, .opts = list(), ...)
}
\arguments{
  \item{x}{a BibEntry object}

  \item{.opts}{a list of formatting options from
  \code{\link{BibOptions}}.  Possible options are \itemize{
  \item \code{style} - character string naming the printing
  style.  Possible values are plain text (style
  \dQuote{text}), BibTeX (\dQuote{Bibtex}), BibLaTeX
  (\dQuote{Biblatex}), a mixture of plain text and BibTeX
  as traditionally used for citations (\dQuote{citation}),
  HTML (\dQuote{html}), LaTeX (\dQuote{latex}),
  \dQuote{markdown}, \dQuote{yaml} (see Note), R code
  (\dQuote{R}), and a simple copy of the textVersion
  elements (style \dQuote{textVersion}, see
  \code{\link{BibEntry}}) \item \code{bib.style} -
  character string specifying BibLaTeX style to use for
  formatting references.  Possible values are
  \dQuote{numeric} (default), \dQuote{authoryear},
  \dQuote{authortitle}, \dQuote{alphabetic},
  \dQuote{draft}.  See section 3.3.2 of the BibLaTeX
  manual. \item \code{sorting} - how should the entries in
  \code{x} be sorted?  See \code{\link{sort.BibEntry}}.
  \item \code{max.names} - maximum number of names to
  display for name list fields before truncation with
  \dQuote{et al.}. \item \code{first.inits} - logical; if
  true only initials of given names are printed, otherwise
  full names are used. \item \code{dashed} - logical; for
  \code{.bibstyle = "authoryear"} or \code{.bibstyle =
  "authoryear"} only, if \code{TRUE} duplicate author and
  editor lists are replaced with \dQuote{---} when printed.
  \item \code{no.print.fields} character vector; fields
  that should not be printed, e.g., doi, url, isbn, etc. }}

  \item{...}{extra parameters to pass to the renderer.}
}
\description{
Prints bibliographic information stored in BibEntry objects
in BibLaTeX style
}
\note{
setting max.names to \code{value} is equivalent to setting
\code{maxnames=value} and \code{minnames=value} in
BibLaTeX.

Custom BibLaTeX styles may be defined using the function
\code{\link{bibstyle}}.  To fully support BibLaTeX, the
created environment must have functions for formatting each
of the entry types decribed in \code{\link{BibEntry}}.

\code{style = "yaml"} is only available if the user has
downloaded a version of \code{pandoc} that includes
\code{pandoc-citeproc}.  In this case, a temporary
\code{.bib} file will be written and processed by
\code{pandoc-citeproc}, the results of which will be
outputted.
}
\examples{
file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- suppressMessages(ReadBib(file.name))
print(bib[author="aristotle"], .opts = list(bib.style = "numeric"))
print(bib[55:57], .opts = list(bib.style = "authortitle", first.inits = FALSE))
print(bib[80:88], .opts = list(bib.style = "alphabetic", max.names = 1, no.print.fields = "issn"))
print(bib[32:36], .opts = list(bib.style = "draft"))
oldopts <- BibOptions(bib.style = "authoryear", dashed = TRUE, sorting = "ydnt")
bib[editor = "westfahl"]
BibOptions(oldopts)
}
\references{
Lehman, Philipp and Kime, Philip and Boruvka, Audrey and
Wright, J. (2013). The biblatex Package.
\url{http://ctan.mirrorcatalogs.com/macros/latex/contrib/biblatex/doc/biblatex.pdf}.
}
\seealso{
\code{\link{BibEntry}}, \code{\link{ReadBib}},
\code{\link{sort.BibEntry}}
}

