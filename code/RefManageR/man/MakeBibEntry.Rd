\name{MakeBibEntry}
\alias{MakeBibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeBibEntry(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    type <- attr(x, "entry")
    key <- attr(x, "key")
    y <- as.list(x)
    names(y) <- tolower(names(y))
    if ("author" \%in\% names(y)) {
        y[["author"]] <- ArrangeAuthors(y[["author"]])
    }
    if ("editor" \%in\% names(y)) {
        y[["editor"]] <- ArrangeAuthors(y[["editor"]])
    }
    if ("date" \%in\% names(y)) {
        y[["date"]] <- as.Date(switch(as.character(nchar(y[["date"]])), 
            `4` = paste0(y[["date"]], "-01-01"), `7` = paste0(y[["date"]], 
                "-01"), y[["date"]]))
    }
    else if ("year" \%in\% names(y)) {
        y[["date"]] <- as.Date(paste0(y[["year"]], "-01-01"))
    }
    tryCatch(BibEntry(bibtype = type, key = key, other = y), 
        error = function(e) {
            message(sprintf("ignoring entry '\%s' (line \%d) because :\n\t\%s\n", 
                key, attr(x, "srcref")[1], conditionMessage(e)))
            NULL
        })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
