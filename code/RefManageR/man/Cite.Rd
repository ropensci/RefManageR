\name{Cite}
\alias{AutoCite}
\alias{Cite}
\alias{Citep}
\alias{Citet}
\alias{PrintBibliography}
\alias{TextCite}
\title{Cite a BibEntry object in text and print all citations}
\usage{
Cite(bib, ..., textual = FALSE, before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", ";",
  "a", ",", ","))

PrintBibliography(bib, style = "text", .opts = list(),
  no.print.fields = NULL)

Citep(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", ";",
  "a", ",", ","))

AutoCite(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", ";",
  "a", ",", ","))

Citet(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", ";",
  "a", ",", ","))

TextCite(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", ";",
  "a", ",", ","))
}
\arguments{
  \item{bib}{a BibEntry object}

  \item{...}{passed to SearchBib for indexing into bib.  A
  character vector of keys would be the usual choice.}

  \item{textual}{logical, if TRUE, a \dQuote{textual}
  citation is produce, i.e. what is produced by \\citet in
  \code{natbib} and \\textcite in \code{BibLaTeX}.}

  \item{before}{string; optional text to display before the
  citation.}

  \item{after}{string; optional text to display after the
  citation.}

  \item{cite.style}{character; the citation style to use.
  Possible values are \dQuote{authoryear},
  \dQuote{numbers}, and \dQuote{super}.}

  \item{max.names}{numeric; maximum number of last names to
  print before truncating.}

  \item{longnamesfirst}{logical; should the first time a
  citation appears in the text not be truncated at
  \code{max.names}?}

  \item{bibpunct}{character vector; punctuation to use in
  the citation.  See \code{\link[utils]{cite}}}

  \item{style}{character; see
  \code{\link{print.BibEntry}}.}

  \item{.opts}{list of formatting options; see
  \code{\link{print.BibEntry}}.}

  \item{no.print.fiels}{character vector; fields that
  should not be printed, e.g., doi, url, isbn, etc.}
}
\value{
For the cite functions: a character string containing the
citation

PrintBibliography: The formatted list of references.
}
\description{
The Cite function allows for citing a BibEntry object in
text.  The PrinBibliography function allows for printing
the bibliography of all the cited entries.  These functions
are most useful when used in , e.g., a RMarkdown or RHTML
document.
}
\examples{
\dontrun{
library(knitr)
doc <- system.file("Rmd", "rmdExample.Rmd", package = "RefManageR")
file.show(doc)
tmpfile <- tempfile(fileext = ".html")
knit(doc, tmpfile)
browseURL(tmpfile)
unlink(tmpfile)
}
}
\seealso{
\code{\link{print.BibEntry}},
\code{\link[utils]{citeNatbib}}
}

