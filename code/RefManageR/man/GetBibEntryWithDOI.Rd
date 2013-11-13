\name{GetBibEntryWithDOI}
\alias{GetBibEntryWithDOI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetBibEntryWithDOI(doi, temp.file = "tempfile.bib", delete.file = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doi}{
%%     ~~Describe \code{doi} here~~
}
  \item{temp.file}{
%%     ~~Describe \code{temp.file} here~~
}
  \item{delete.file}{
%%     ~~Describe \code{delete.file} here~~
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
function (doi, temp.file = "tempfile.bib", delete.file = TRUE) 
{
    file.create(temp.file)
    for (i in 1:length(doi)) {
        temp <- getURLContent(url = paste0("http://dx.doi.org/", 
            doi), .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"), 
            followLocation = TRUE))
        if (is.raw(temp)) 
            temp <- rawToChar(temp)
        write(temp, file = temp.file, append = TRUE)
    }
    bib.res <- read.bib(file = temp.file, encoding = "UTF-8")
    if (delete.file) 
        suppressWarnings(file.remove(temp.file))
    bib.res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
