\name{levels.BibEntry}
\alias{levels.BibEntry}
\alias{fields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get all BibTeX/BibLaTeX fields in a BibEntry object
}
\description{
Returns the fields that have been defined for each entry in \code{x}.
}
\usage{
\method{levels}{BibEntry}(x)

fields(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class BibEntry
}
}
\details{
\code{fields} and \code{levels} perform the same function except the list returned by levels has names corresponding to the keys of the BibTeX entries.
}
\value{
A named list.  Each entry in the list is a character vector of the fields defined for the corresponding entry in \code{x}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
McLean, M. W.
}
\note{
\code{\sQuote{key}} and \code{\sQuote{bibtype}} are never included in the returned list since they must be present for every entry.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    return(lapply(unclass(x), names))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
