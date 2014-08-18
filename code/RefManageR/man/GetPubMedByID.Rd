\name{GetPubMedByID}
\alias{GetPubMedByID}
\title{Retrieve citation information from NCBI's Entrez for a set of PubMed IDs}
\usage{
GetPubMedByID(id, db = "pubmed", ...)
}
\arguments{
  \item{id}{character vector; PubMed ID's for searching
  NCBI's Entrez.}

  \item{db}{string; Entrez database to search.}

  \item{...}{additional parameters to use for the search.
  See the Entrez documentation listed in the
  \emph{References}.}
}
\value{
a BibEntry object.
}
\description{
Uses NCBI's E-Utilities to retrieve bibliographic
information given a vector of PubMed ID's and returns the
results as a BibEntry object.
}
\note{
If journal information is returned for an ID, then an entry
with \code{bibtype} \dQuote{Article} will be created;
otherwise, the \code{bibtype} will be \dQuote{Misc}.
}
\examples{
if (interactive() && url.exists("http://eutils.ncbi.nlm.nih.gov/"))
  GetPubMedByID(c("11209037", "21245076"))
}
\references{
\url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}
}
\seealso{
Other pubmed: \code{\link{GetDOIs}};
\code{\link{GetPubMedRelated}};
\code{\link{LookupPubMedID}}; \code{\link{ReadCrossRef}};
\code{\link{ReadPubMed}}
}
\keyword{database}

