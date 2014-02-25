\name{WriteBib}
\alias{WriteBib}
\title{Create a BibTeX File from a BibEntry Object}
\usage{
WriteBib(bib, file = "references.bib", biblatex = TRUE, append = FALSE,
  verbose = TRUE, ...)
}
\arguments{
  \item{bib}{a BibEntry object to be written to file}

  \item{file}{character string naming a file, should; end
  in \dQuote{.bib}}

  \item{biblatex}{boolean; if \code{TRUE},
  \code{\link{toBiblatex}} is used and no conversions of
  the BibEntry object are done; if \code{FALSE} entries
  will be converted as described in
  \code{\link{toBibtex.BibEntry}}.}

  \item{append}{as in \code{\link{write.bib}}}

  \item{verbose}{as in \code{\link{write.bib}}}

  \item{...}{additional arguments passed to
  \code{\link{writeLines}}}
}
\value{
\code{bib} - invisibly
}
\description{
Creates a Bibtex File from a BibEntry object for use with
either BibTeX or BibLaTex.
}
\note{
To write the contents of \code{bib} \dQuote{as is}, the
argument \code{biblatex} should be \code{TRUE}, otherwise
conversion is done as in \code{\link{toBibtex.BibEntry}}.
}
\examples{
bib <- ReadCrossRef(query = 'carroll journal of the american statistical association',
  year = 2012)
tfile <- tempfile(fileext = ".bib")
WriteBib(bib, tfile, biblatex = TRUE)
identical(ReadBib(tfile), bib)
unlink(tfile)
}
\author{
McLean, M. W. - based on \code{write.bib} function in
package \code{bibtex} by Francois, R.

McLean, M. W. based on \code{\link{write.bib}} by Gaujoux,
R. in package \code{bibtex}.
}
\seealso{
\code{\link{write.bib}}, \code{\link{ReadBib}},
\code{\link{toBibtex.BibEntry}}, \code{\link{toBiblatex}},
\code{\link{BibEntry}}
}
\keyword{IO}

