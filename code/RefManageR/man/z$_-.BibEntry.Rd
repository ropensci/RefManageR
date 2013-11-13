\name{$<-.BibEntry}
\alias{$<-.BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
$<-.BibEntry(x, name, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
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
function (x, name, value) 
{
    is_attribute <- name \%in\% bibentry_attribute_names
    x <- unclass(x)
    name <- tolower(name)
    value <- rep(.listify(value), length.out = length(x))
    if (name == "bibtype") {
        stopifnot(all(sapply(value, length) == 1L))
        BibTeX_names <- names(BibLaTeX_entry_field_db)
        value <- unlist(value)
        pos <- match(tolower(value), tolower(BibTeX_names))
        if (any(is.na(pos))) 
            stop(gettextf("\%s has to be one of \%s", sQuote("bibtype"), 
                paste(BibTeX_names, collapse = ", ")), domain = NA)
        value <- as.list(BibTeX_names[pos])
    }
    for (i in seq_along(x)) {
        if (is_attribute) {
            attr(x[[i]], name) <- if (is.null(value[[i]])) 
                NULL
            else paste(value[[i]])
        }
        else {
            x[[i]][[name]] <- if (is.null(value[[i]])) 
                NULL
            else {
                if (name \%in\% c("author", "editor")) 
                  as.person(value[[i]])
                else paste(value[[i]])
            }
        }
    }
    for (i in seq_along(x)) .BibEntryCheckBibEntry1(x[[i]])
    class(x) <- "bibentry"
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
