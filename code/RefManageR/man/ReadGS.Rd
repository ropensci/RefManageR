\name{ReadGS}
\alias{ReadGS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import book and article references from a public Google Scholar profile by ID.
}
\description{
This function will create a BibEntry object for up to 100 references from a provided Google Scholar ID, if the profile is public.  The number of citations for each entry will also be imported.
}
\usage{
GSImport(scholar.id, start = 0, limit = 100, sort.by.date = FALSE, .Encoding = "UTF-8", bib.violation = .BibOptions$bib.violation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scholar.id}{
character; the Google Scholar ID from which citations will be imported.  The ID can by found by visiting an author's Google Scholar profile and noting the value in the uri for the \dQuote{user} parameter.
}
  \item{start}{
numeric; index of first citation to include.
}
  \item{limit}{
numeric; maximum number of results to return.  Cannot be more than 100.
}
  \item{sort.by.date}{
  boolean; if true, newest citations are imported first; otherwise, most cited works are imported first.
}
  \item{.Encoding}{
character; text encoding to use for importing the results and creating the bib entries.
}
  \item{check.entries}{
character or logical; If there is an error in the format of a citation (i.e. it is missing a field required for that entry type), how should the entry be handled?  If \code{check.entry = \sQuote{error}} then the entry is not added.  If \code{check.entry = \sQuote{warn}}, the entry is added with a warning message.  If \code{check.entry = FALSE}, entries are not checked.  The default is set based on the current value of the parameter in .BibOptions.  See \code{Details}.}
}
}
\details{
This function creates \sQuote{Article} or \sQuote{Book} BibTeX entries from an author's Google Scholar page.  If the function finds numbers corresponding to volume/number/pages of a journal article, an \sQuote{Article} entry is created; otherwise, a \sQuote{Book} entry is created.

Long author lists, long titles, and long journal/publisher names can all lead to these fields being incomplete for a particular entry.  When this occurs, these entries are either dropped or added with warning depending on the value of the \code{check.entries} argument.

It is not possible to automatically import BibTeX entries directly from Google Scholar as no API is available and this violates their Terms of Service.
}
\value{
An object of class BibEntry.  If the entry has any citations, the number of citations is stored in a field \sQuote{cites}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
McLean, M. W.
}
\note{
%%  ~~further notes~~
}

\section{Warning }{Read Google's Terms of Service before using.  Use at own risk.}

\seealso{
\code{\link{BibEntry}}, \code{\link{BibEntry}}, \code{get_publications} in package \code{scholar}
}
\examples{
## R. J. Carroll's ten newest publications
ReadGS(scholar.id = "CJOHNoQAAAAJ", limit = '10', sort.by.date = TRUE)

## Matthias Katzfu\ss
kat.bib <- ReadGS(scholar.id = 'vqW0UqUAAAAJ')

## retrieve GS citation counts stored in field 'cites'
## entries with no citations do not have the cite field
kat.bib['cites'] 
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ database }
