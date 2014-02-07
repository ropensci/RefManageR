\name{ReadGS}
\alias{ReadGS}
\title{Import book and article references from a public Google Scholar profile by ID.}
\usage{
ReadGS(scholar.id, start = 0, limit = 100, sort.by.date = FALSE,
  .Encoding = "UTF-8", check.entries = BibOptions()$check.entries)
}
\arguments{
  \item{scholar.id}{character; the Google Scholar ID from
  which citations will be imported.  The ID can by found by
  visiting an author's Google Scholar profile and noting
  the value in the uri for the \dQuote{user} parameter.}

  \item{start}{numeric; index of first citation to
  include.}

  \item{limit}{numeric; maximum number of results to
  return.  Cannot exceed 100.}

  \item{sort.by.date}{boolean; if true, newest citations
  are imported first; otherwise, most cited works are
  imported first.}

  \item{.Encoding}{character; text encoding to use for
  importing the results and creating the bib entries.}

  \item{check.entries}{What should be done with incomplete
  entries (those containing \dQuote{...} due to long
  fields)? Either \code{FALSE} to add them anyway,
  \code{"warn"} to add with a warning, or any other value
  to drop the entry with a message and continue processing
  the remaining entries.}
}
\value{
An object of class BibEntry.  If the entry has any
citations, the number of citations is stored in a field
\sQuote{cites}.
}
\description{
This function will create a BibEntry object for up to 100
references from a provided Google Scholar ID, if the
profile is public.  The number of citations for each entry
will also be imported.
}
\details{
This function creates \sQuote{Article} or \sQuote{Book}
BibTeX entries from an author's Google Scholar page. If the
function finds numbers corresponding to volume/number/pages
of a journal article, an \sQuote{Article} entry is created;
otherwise, a \sQuote{Book} entry is created.

Long author lists, long titles, and long journal/publisher
names can all lead to these fields being incomplete for a
particular entry.  When this occurs, these entries are
either dropped or added with warning depending on the value
of the \code{check.entries} argument.
}
\note{
Read Google's Terms of Service before using.

It is not possible to automatically import BibTeX entries
directly from Google Scholar as no API is available and
this violates their Terms of Service.
}
\examples{
## R. J. Carroll's ten newest publications
ReadGS(scholar.id = "CJOHNoQAAAAJ", limit = 10, sort.by.date = TRUE)

## Matthias Katzfu\\ss
kat.bib <- ReadGS(scholar.id = 'vqW0UqUAAAAJ')

## retrieve GS citation counts stored in field 'cites'
## entries with no citations do not have the cite field
kat.bib['cites']
}
\seealso{
\code{\link{BibEntry}}, \code{\link{BibEntry}},
\code{get_publications} in package \code{scholar}
}
\keyword{database}

