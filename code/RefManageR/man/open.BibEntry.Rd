\name{open.BibEntry}
\alias{open.BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Open BibEntry in PDF viewer or web browser.
}
\description{
Opens a pdf copy for a single BibEntry, or opens a url for the BibEntry in a WWW browser if they are available.
}
\usage{
open(bib, entry = 1, open.field = c("file", "url", "eprint", "doi"), viewer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bib}{
object of class BibEntry.
}
  \item{entry}{
numeric index or character key of entry in \code{bib} to open.
}
  \item{open.field}{
character vector of fields to use in \code{bib} to open the BibEntry.  Possible fields are any combination of \dQuote{file},\dQuote{url}, \dQuote{eprint}, or \dQuote{doi}.  \dQuote{eprint} is implemented for \code{eprinttype=} \dQuote{JSTOR}, \dQuote{PubMed}, or \dQuote{arXiv}.  When multiple fields are specified, they are tried in the order they appear in the vector.
}
  \item{viewer}{
character string giving the name of the program to be used as hypertext browser. It should be in the PATH, or a full path specified. Alternatively, an R function to be called to invoke the browser.  Defaults to \code{getOptions(\dQuote{pdfviewer})} if \code{open.field = \dQuote{file}} and \code{getOptions(\dQuote{browser})}, otherwise.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
none
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{browseURL}}
}
\examples{
\dontrun{
testbib <- ReadBib(system.file("REFERENCES.bib", package="bibtex"))
open(testbib)
testbib$file <- file.path(paste0(R.home(), '/doc/manual/R-intro.pdf'))
open(testbib)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{connection}
\keyword{utilities}
