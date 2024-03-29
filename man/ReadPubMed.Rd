% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadPubMed.R
\name{ReadPubMed}
\alias{ReadPubMed}
\title{Search NCBI's E-Utilities for citation information}
\usage{
ReadPubMed(query, database = "PubMed", ...)
}
\arguments{
\item{query}{string; search term.}

\item{database}{string; the Entrez database to search.}

\item{...}{additional parameters to use for the search.
See the \emph{Details}.}
}
\value{
an object of class BibEntry.
}
\description{
This function takes a query and searches an Entrez database for
references using NCBI's E-Utilities, returning the results in a BibEntry
object.
}
\details{
Optional additional parameters to pass to the server include
\itemize{
\item \code{retstart} - index of the first retrieved ID that should be
included in the results.
\item \code{retmax} - maximum number of IDs the server will
return (default 20).
\item \code{field} - limits the query to search only the specified
field (e.g. \dQuote{title}).
\item \code{datetype} - type of date to use when limiting search by
dates. E.g. \dQuote{mdat}
for modification date or \dQuote{pdat} for publication date.
\item \code{reldate} - integer; only items that have (\code{datetype})
date values within \code{reldate} \emph{days}
of the current date will be returned.
\item \code{mindate}, \code{maxdate} - date ranges to restrict search
results.  Possible formats are
\dQuote{YYYY}, \dQuote{YYYY/MM}, and \dQuote{YYYY/MM/DD}.
}
}
\note{
The returned entries will have type either \sQuote{Article} or
\sQuote{Misc} depending on whether journal information was retrieved.
See the Entrez documentation listed in the \emph{References}.

The language of the entry will be returned in the field \dQuote{language}
and the abstract will be returned in the field \dQuote{abstract}, if they
are available.
}
\examples{
if (interactive() && !httr::http_error("https://eutils.ncbi.nlm.nih.gov/"))
  ReadPubMed(query = "raymond carroll measurement error", retmax = 5, mindate = 1990)
}
\references{
\url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}
}
\seealso{
Other pubmed: 
\code{\link{GetPubMedByID}()},
\code{\link{GetPubMedRelated}()},
\code{\link{LookupPubMedID}()},
\code{\link{ReadCrossRef}()}
}
\concept{pubmed}
