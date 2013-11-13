\name{+.BibEntry}
\alias{+.BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
+.BibEntry(e1, e2, duplicate.fields = .BibOptions$duplicate.fields)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
%%     ~~Describe \code{e1} here~~
}
  \item{e2}{
%%     ~~Describe \code{e2} here~~
}
  \item{duplicate.fields}{
%%     ~~Describe \code{duplicate.fields} here~~
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
function (e1, e2, duplicate.fields = .BibOptions$duplicate.fields) 
{
    dup.ind <- match(names(e2), names(e1), nomatch = FALSE)
    good.ind <- 1:length(e2)
    message(paste0("Repeated keys in ", deparse(substitute(e2)), 
        " for entries"))
    message(paste0(dup.ind, " "))
    if ("key" \%in\% duplicate.fields) {
        message("These entries will be dropped in result")
        duplicate.fields <- duplicate.fields[duplicate.fields != 
            "key"]
        e2 <- e2[-dup.ind]
        good.ind <- good.ind[-dup.ind]
    }
    else {
        message("Adding '2' to end of these keys in result")
        names(e2)[dup.ind] <- paste0(names(e2)[dup.ind], "2")
        dup.ind <- NULL
    }
    curopt <- .BibOptions$return.ind
    .BibOptions$return.ind <- TRUE
    current.fields <- c(unique(c(names(unlist(e1)), names(unlist(e2)))), 
        "bibtype")
    if (length(duplicate.fields)) {
        for (i in 1:length(duplicate.fields)) {
            curfield <- duplicate.fields[i]
            if (curfield \%in\% current.fields) {
                cur.ind <- unlist(lapply(eval(parse(text = paste0("e2$", 
                  curfield))), function(x) suppressMessages(e1[curfield, 
                  x])))
                dup.ind <- c(dup.ind, cur.ind)
                if (length(dup.ind) == length(e2)) {
                  message(paste0("Only duplicates in ", deparse(substitute(e2))))
                  return(e1)
                }
                e2 <- e2[-cur.ind]
                message(paste0("Repeated entries found in ", 
                  deparse(subsitute(e2)), " when examining ", 
                  curfield, "field for entries"))
                message(paste0(good.ind[cur.ind], " "))
                good.ind <- good.ind[-cur.ind]
            }
            else {
                paste0(curfield, " is not a field in any entries.  Ignoring.")
            }
        }
    }
    .BibOptions$return.ind <- curopt
    c(e1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
