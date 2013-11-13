\name{MatchDate}
\alias{MatchDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MatchDate(x, pattern)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
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
function (x, pattern) 
{
    match.date <- "yearonly"
    x <- unclass(x)
    if (length(grep("--", pattern))) {
        pos <- regexpr("--", pattern)[1]
        str1 <- substr(pattern, 1, pos - 1)
        d2 <- as.Date(switch(as.character(nchar(str1)), `4` = paste0(str1, 
            "-01-01"), `7` = paste0(str1, "-01"), str1))
        if (match.date != "exact") 
            d2 <- year(d2)
        match.pos <- sapply(x, CompareDates, date2 = d2, compare.op = ">=")
        if (sum(match.pos) == 0) {
            print("No matches")
            return()
        }
        x <- x[match.pos]
        str1 <- substr(pattern, pos + 2, nchar(pattern))
        comp.op <- "<="
    }
    else if ((comp.op <- substr(pattern, 1, 2)) == "<=" || comp.op == 
        ">=") {
        str1 <- substr(pattern, 3, nchar(pattern))
    }
    else if ((comp.op <- substr(pattern, 1, 1)) == ">" || comp.op == 
        "<") {
        str1 <- substr(pattern, 2, nchar(pattern))
    }
    else {
        comp.op <- "=="
        str1 <- pattern
    }
    d2 <- as.Date(switch(as.character(nchar(str1)), `4` = paste0(str1, 
        "-01-01"), `7` = paste0(str1, "-01"), str1))
    if (match.date != "exact") 
        d2 <- year(d2)
    match.pos <- sapply(x, CompareDates, date2 = d2, compare.op = comp.op)
    if (sum(match.pos) == 0) {
        print("No Matches")
        return()
    }
    return(match.pos)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
