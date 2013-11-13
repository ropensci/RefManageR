\name{relist.BibEntry}
\alias{relist.BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
relist.BibEntry(flesh, skeleton = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flesh}{
%%     ~~Describe \code{flesh} here~~
}
  \item{skeleton}{
%%     ~~Describe \code{skeleton} here~~
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
function (flesh, skeleton = NULL) 
{
    key.ind <- which(names(flesh) == "key")
    N <- length(key.ind)
    res <- vector("list", N)
    res[[1]] <- structure(flesh[1:(key.ind[1] - 2)], bibtype = as.character(flesh[key.ind[1] - 
        1]), key = as.character(flesh[key.ind[1]]))
    if (N > 1) {
        for (i in 2:N) {
            res[[i]] <- structure(flesh[(key.ind[i - 1] + 1):(key.ind[i] - 
                2)], bibtype = as.character(flesh[key.ind[i] - 
                1]), key = as.character(flesh[key.ind[i]]))
        }
    }
    class(res) <- c("BibEntry", "bibentry")
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
