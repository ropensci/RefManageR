\name{CompareDates}
\alias{CompareDates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CompareDates(bib.entry, date2, compare.op = "==", match.date = "year.only")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bib.entry}{
%%     ~~Describe \code{bib.entry} here~~
}
  \item{date2}{
%%     ~~Describe \code{date2} here~~
}
  \item{compare.op}{
%%     ~~Describe \code{compare.op} here~~
}
  \item{match.date}{
%%     ~~Describe \code{match.date} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
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
function (bib.entry, date2, compare.op = "==", match.date = "year.only") 
{
    date1 <- bib.entry$date
    if (is.null(date1)) 
        return(FALSE)
    if (match.date != "exact") {
        date1 <- year(date1)
    }
    return(do.call(compare.op, list(date1, date2)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
