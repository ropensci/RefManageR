\name{GetBibEntryWithDOI}
\alias{GetBibEntryWithDOI}
\title{Lookup a Bibtex entry using a Digital Object Identifier}
\usage{
GetBibEntryWithDOI(doi, temp.file = tempfile(fileext = ".bib"),
  delete.file = TRUE)
}
\arguments{
  \item{doi}{character vector; DOIs to use to retrieve
  bibliographic information.}

  \item{temp.file}{string; a file to write the Bibtex data
  returned by the DOI System to.}

  \item{delete.file}{logical; should \code{temp.file} be
  deleted when the function exits?}
}
\value{
an object of class BibEntry.
}
\description{
Uses the DOI System API to look up bibliography information
given a set of DOIs.
}
\details{
The bibliographic information returned by the search of the
\url{http://dx.doi.org} API is temporarily written to a
file and then read back into \code{R} and return as a
\code{BibEntry} object.
}
\examples{
if (interactive() && url.exists("http://dx.doi.org/"))
  GetBibEntryWithDOI(c("10.1016/j.iheduc.2003.11.004", "10.3998/3336451.0004.203"))
}
\references{
\url{http://www.doi.org/tools.html}
}
\seealso{
\code{\link{ReadCrossRef}}, \code{\link{BibEntry}}
}

