\name{ReadCrossRef}
\alias{ReadCrossRef}
\title{Search CrossRef for citations.}
\usage{
ReadCrossRef(query, limit = 5, sort = "relevance", year = NULL,
  min.relevance = 80, temp.file = tempfile(fileext = ".bib"),
  delete.file = TRUE, verbose = FALSE)
}
\arguments{
  \item{query}{- string; search term}

  \item{limit}{- numeric; maximum number of entries to
  return}

  \item{sort}{- string; how should the results from
  CrossRef be returned.}

  \item{year}{- numeric; if specified, only results from
  this year will be returned.}

  \item{min.relevance}{- numeric; only results with a
  CrossRef-assigned relevance score at least this high will
  be returned.}

  \item{temp.file}{- string; file name to use for storing
  Bibtex information returned by CrossRef.}

  \item{delete.file}{- boolean; should the bib file be
  deleted on exit?}

  \item{verbose}{- boolean; if \code{TRUE}, additional
  messages are output regarding the results of the query.}
}
\value{
An object of class BibEntry.
}
\description{
Provides an interface to the CrossRef API, searching for
citations given a string query.  Results are written to a
bib file, read back into \code{R} using
\code{\link{WriteBib}}, and returned as a BibEntry object.
}
\details{
CrossRef assigns a score between 0 and 100 based on how
relevant a reference seems to be to your query.  The API
documenation warns that while false negatives are unlikely,
the search can be prone to false positives.  Hence, setting
\code{min.revelance} to a high value may be necessary.
}
\examples{
ReadCrossRef(query = 'rj carroll measurement error', limit = 2, sort = "relevance",
  min.relevance = 80)

ReadCrossRef(query = 'carroll journal of the american statistical association',
  year = 2012, limit = 2)
}
\references{
\url{http://search.crossref.org/help/api}
}
\seealso{
\code{\link{ReadZotero}}, \code{\link{BibEntry}}

Other pubmed: \code{\link{GetPubMedByID}};
\code{\link{GetPubMedRelated}};
\code{\link{LookupPubMedID}}; \code{\link{ReadPubMed}}
}
\keyword{database}

