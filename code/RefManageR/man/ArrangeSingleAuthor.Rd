\name{ArrangeSingleAuthor}
\alias{ArrangeSingleAuthor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ArrangeSingleAuthor(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (y) 
{
    if (grepl(",", y)) {
        y <- sub("^([^,]+)[[:space:]]*,[[:space:]]*(.*?)$", "\\2 \\1", 
            y, perl = TRUE)
    }
    rx <- "^[{](.*)[}]$"
    rx2 <- "^([^]]*)[{]([^]]*)[}]$"
    if (grepl(rx, y)) {
        person(sub(rx, "\\1", y))
    }
    else if (grepl(rx2, y)) {
        person(sub(rx2, "\\1", y), sub(rx2, "\\2", y))
    }
    else {
        as.person(y)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
