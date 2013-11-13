\name{BibEntry}
\alias{BibEntry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BibEntry(bibtype, textVersion = NULL, header = NULL, footer = NULL, key = NULL, ..., other = list(), mheader = NULL, mfooter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bibtype}{
%%     ~~Describe \code{bibtype} here~~
}
  \item{textVersion}{
%%     ~~Describe \code{textVersion} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{footer}{
%%     ~~Describe \code{footer} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{other}{
%%     ~~Describe \code{other} here~~
}
  \item{mheader}{
%%     ~~Describe \code{mheader} here~~
}
  \item{mfooter}{
%%     ~~Describe \code{mfooter} here~~
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
function (bibtype, textVersion = NULL, header = NULL, footer = NULL, 
    key = NULL, ..., other = list(), mheader = NULL, mfooter = NULL) 
{
    BibTeX_names <- names(BibLaTeX_entry_field_db)
    args <- c(list(...), other)
    if (!length(args)) 
        return(structure(list(), class = "bibentry"))
    if (any(sapply(names(args), .is_not_nonempty_text))) 
        stop("all fields have to be named")
    args <- c(list(bibtype = bibtype, textVersion = textVersion, 
        header = header, footer = footer, key = key), list(...))
    args <- lapply(args, .listify)
    other <- lapply(other, .listify)
    max_length <- max(sapply(c(args, other), length))
    args_length <- sapply(args, length)
    if (!all(args_length_ok <- args_length \%in\% c(1L, max_length))) 
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: \%s", 
            paste(names(args)[!args_length_ok], collapse = ", ")), 
            domain = NA)
    args <- lapply(args, function(x) rep(x, length.out = max_length))
    other_length <- sapply(other, length)
    if (!all(other_length_ok <- other_length \%in\% c(1L, max_length))) 
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: \%s", 
            paste(names(other)[!other_length_ok], collapse = ", ")), 
            domain = NA)
    other <- lapply(other, function(x) rep(x, length.out = max_length))
    bibentry1 <- function(bibtype, textVersion, header = NULL, 
        footer = NULL, key = NULL, ..., other = list()) {
        bibtype <- as.character(bibtype)
        stopifnot(length(bibtype) == 1L)
        pos <- match(tolower(bibtype), tolower(BibTeX_names))
        if (is.na(pos)) 
            stop(gettextf("\%s has to be one of \%s", sQuote("bibtype"), 
                paste(BibTeX_names, collapse = ", ")), domain = NA)
        bibtype <- BibTeX_names[pos]
        rval <- c(list(...), other)
        rval <- rval[!sapply(rval, .is_not_nonempty_text)]
        fields <- tolower(names(rval))
        names(rval) <- fields
        attr(rval, "bibtype") <- bibtype
        .BibEntryCheckBibEntry1(rval)
        pos <- fields \%in\% c("author", "editor")
        if (any(pos)) {
            for (i in which(pos)) rval[[i]] <- as.person(rval[[i]])
        }
        if (any(!pos)) {
            for (i in which(!pos)) rval[[i]] <- as.character(rval[[i]])
        }
        attr(rval, "key") <- if (is.null(key)) 
            NULL
        else as.character(key)
        if (!is.null(textVersion)) 
            attr(rval, "textVersion") <- as.character(textVersion)
        if (!.is_not_nonempty_text(header)) 
            attr(rval, "header") <- paste(header, collapse = "\n")
        if (!.is_not_nonempty_text(footer)) 
            attr(rval, "footer") <- paste(footer, collapse = "\n")
        return(rval)
    }
    rval <- lapply(seq_along(args$bibtype), function(i) do.call("bibentry1", 
        c(lapply(args, "[[", i), list(other = lapply(other, "[[", 
            i)))))
    if (!.is_not_nonempty_text(mheader)) 
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if (!.is_not_nonempty_text(mfooter)) 
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")
    class(rval) <- c("BibEntry", "bibentry")
    rval
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
