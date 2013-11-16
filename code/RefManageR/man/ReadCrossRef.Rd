\name{ReadCrossRef}
\alias{ReadCrossRef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import Bibliographic Information Using CrossRef Metadata Search
}
\description{
Uses the CrossRef API to search for BibTeX entries.  Results are written to a temporary \code{.bib} file that is processed by \code{\link{ReadBib}} to return a BibEntry object.
}
\usage{
ReadCrossRef(query, limit = 5, sort = "relevance", year = NULL, min.relevance = 80, 
             temp.file = tempfile(fileext = '.bib'), delete.file = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{query}{
character; search string to use to query CrossRef.
}
  \item{limit}{
numeric; maximum of results to return.
}
  \item{sort}{
character; how should results be sorted?  Possible values are \sQuote{year} or \sQuote{relevance}.
}
  \item{year}{
numeric; if specified, search is only conducted for references from this year.
}
  \item{min.relevance}{
numeric. CrossRef assigns a relevance score for each entry (with a maximum value of 100) based on how likely it is that result is a match for the specified \code{query}.  Entries with relevance score less than \code{min.relevance} with not be added to the output.
}
  \item{temp.file}{
character; name of a temporary file for storing results.
}
  \item{delete.file}{
boolean; should the temporary \code{.bib} file be deleted?
}
  \item{verbose}{
boolean; if true, a message is printed with full citation and relevancy score of each result to be included in output.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class BibEntry.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
McLean, M. W.
}
\note{
The CrossRef Metadata Search will almost always produce a match, false positives are common.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ReadBib}}
}
\examples{
ReadCrossRef(query = 'rj carroll measurement error', limit = 5, sort = "relevance", 
             min.relevance = 80)
             
ReadCrossRef(query = 'carroll journal of the american statistical association', year = 2012)             
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{database}
