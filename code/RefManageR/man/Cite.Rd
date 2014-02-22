\name{NoCite}
\alias{AutoCite}
\alias{Cite}
\alias{Citep}
\alias{Citet}
\alias{NoCite}
\alias{PrintBibliography}
\alias{TextCite}
\title{Cite a BibEntry object in text and print all citations}
\usage{
NoCite(bib, ...)

Cite(bib, ..., textual = FALSE, before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, super = FALSE,
  max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", "[",
  "]", ";", ","))

PrintBibliography(bib, style = "text", .opts = list(),
  no.print.fields = NULL)

Citep(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, super = FALSE,
  max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", "[",
  "]", ";", ","))

AutoCite(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, super = FALSE,
  max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", "[",
  "]", ";", ","))

Citet(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, super = FALSE,
  max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", "[",
  "]", ";", ","))

TextCite(bib, ..., before = NULL, after = NULL,
  cite.style = BibOptions()$cite.style, super = FALSE,
  max.names = BibOptions()$max.names,
  longnamesfirst = BibOptions()$longnamesfirst, bibpunct = c("(", ")", "[",
  "]", ";", ","))
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

  \item{cite.style}{character string; bibliography style to
  use to generate citations. See \code{\link{BibOptions}}.}

  \item{super}{logical; should superscripts be used for
  numeric citations?  Ignored if \code{cite.style !=
  "numeric"}.}

  \item{max.names}{numeric; maximum number of last names to
  print before truncating.}

  \item{longnamesfirst}{logical; should the first time a
  citation appears in the text not be truncated at
  \code{max.names}?}

  \item{bibpunct}{character vector; punctuation to use in
  the citation. See the Details.}

  \item{style}{character; see
  \code{\link{print.BibEntry}}.}

  \item{.opts}{list of formatting options; see
  \code{\link{print.BibEntry}}.}

  \item{no.print.fields}{character vector; fields that
  should not be printed, e.g., doi, url, isbn, etc.}
}
\value{
NoCite: no return value; invoked for its side-effect.

For the cite functions: a character string containing the
citation

PrintBibliography: The formatted list of references.
}
\description{
The \code{Cite} functions allow for citing a
\code{BibEntry} object in text.  The
\code{PrintBibliography} function allows for printing the
bibliography of all the cited entries.  The \code{NoCite}
function adds references to the bibliography without
including a citation.  These functions are most useful when
used in, e.g., a RMarkdown or RHTML document.
}
\details{
The entries in \code{bibpunct} are as follows \enumerate{
\item The left delimiter for non-alphabetic and non-numeric
citation styles \item The right delimiter for
non-alphabetic and non-numeric citation styles \item The
left delimiter for alphabetic and numeric citation styles
\item The right delimiter for alphabetic and numeric
citation styles \item The separator between references in a
citation. \item Punctuation to go between the author and
year. }

If \code{bib.style = "alphabetic"} or \code{bib.style =
"numeric"}, then sorting needs to be done at the start of
the document prior to using a cite function as sorting is
not done by the Printbibliography function for those styles
(specifying \code{sorting} in \code{.opts} is ignored in
this case).  If no sorting is none, the references are
listed in the order they were cited in for those two
styles.

If the \code{...} argument to NoCite is identical to
\dQuote{*}, then all references in \code{bib} are added to
the bibliography without citations.
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

