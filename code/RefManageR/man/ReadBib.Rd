\name{ReadBib}
\alias{ReadBib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadBib(file = findBibFile(package), package = "bibtex", encoding = "UTF-8", header = if (length(preamble)) paste(preamble, sep = "\n") else "", footer = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{package}{
%%     ~~Describe \code{package} here~~
}
  \item{encoding}{
%%     ~~Describe \code{encoding} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{footer}{
%%     ~~Describe \code{footer} here~~
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
function (file = findBibFile(package), package = "bibtex", encoding = "UTF-8", 
    header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
    footer = "") 
{
    if (!is.character(file)) {
        stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
    }
    srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
        encoding = encoding))
    out <- .External("do_read_bib", file = file, encoding = encoding, 
        srcfile = srcfile)
    at <- attributes(out)
    if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
        out <- lapply(out, MakeBibEntry)
    else out <- list()
    preamble <- at[["preamble"]]
    out <- MakeCitationList(out, header, footer)
    attr(out, "strings") <- at[["strings"]]
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
