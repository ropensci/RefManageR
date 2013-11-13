\name{[.BibEntry}
\alias{[.BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
[.BibEntry(x, ..., return.ind = .BibOptions$return.ind, drop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{return.ind}{
%%     ~~Describe \code{return.ind} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
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
function (x, ..., return.ind = .BibOptions$return.ind, drop = TRUE) 
{
    if (!length(x)) 
        return(x)
    dots <- list(...)
    current.fields <- c(unique(names(unlist(x))), "bibtype", 
        "key")
    if (length(dots) == 1) {
        dot.arg <- dots[[1]]
        if (is.numeric(dot.arg) || is.logical(dot.arg)) {
            return(x[[dot.arg]])
        }
        else if (is.list(dot.arg)) {
            args <- as.list(sapply(1:length(dot.arg), function(i) c(names(dot.arg)[i], 
                dot.arg[[i]])))
            args$x <- x
            return(do.call("[.BibEntry", args))
        }
        else if (is.character(dot.arg)) {
            dot.arg <- tolower(dot.arg)
            if (dot.arg \%in\% current.fields) {
                res <- eval(parse(text = paste0("x$", dot.arg)))
                names(res) <- names(x)
                return(unlist(res))
            }
            else {
                return(x[[names(x) \%in\% dot.arg]])
            }
        }
        else {
            stop("Invalid argument")
        }
    }
    temp <- dots[[1]]
    if (pmatch(temp <- tolower(temp), current.fields, nomatch = 0)) {
        pattern <- dots[[2]]
        if (temp == "author" || temp == "editor") {
            match.pos <- MatchAuthor(x, field = temp, pattern = pattern)
        }
        else if (temp == "year" || temp == "date") {
            match.pos <- MatchDate(x, pattern = pattern)
        }
        else {
            match.pos <- SearchField(x, field = temp, pattern = pattern)
        }
        dots <- dots[-c(1, 2)]
    }
    else {
        stop("Invalid argument")
    }
    if (sum(match.pos) == 0) {
        message("No matches")
        return()
    }
    if (length(dots) > 1) {
        dots$x <- x[match.pos]
        dots$x <- do.call("[.BibEntry", dots)
    }
    if (return.ind) {
        return(which(match.pos))
    }
    else {
        x <- x[match.pos]
        class(x) <- c("BibEntry", "bibentry")
        return(x)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
