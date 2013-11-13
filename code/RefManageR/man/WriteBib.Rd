\name{WriteBib}
\alias{WriteBib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WriteBib(entry, file = "Rpackages.bib", append = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{entry}{
%%     ~~Describe \code{entry} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{append}{
%%     ~~Describe \code{append} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (entry, file = "Rpackages.bib", append = FALSE, verbose = TRUE) 
{
    bibs <- if (inherits(entry, "bibentry")) 
        entry
    else if (is.character(entry)) {
        if (length(entry) == 0) {
            if (verbose) 
                message("Empty package list: nothing to be done.")
            return(invisible())
        }
        pkgs <- entry
        if (is.null(pkgs)) 
            pkgs <- unique(installed.packages()[, 1])
        bibs <- sapply(pkgs, function(x) try(citation(x)), simplify = FALSE)
        n.installed <- length(bibs)
        ok <- sapply(bibs, inherits, "bibentry")
        pkgs <- pkgs[ok]
        bibs <- bibs[ok]
        n.converted <- sum(ok)
        pkgs <- lapply(seq_along(pkgs), function(i) if (length(bibs[[i]]) > 
            1) 
            paste(pkgs[i], 1:length(bibs[[i]]), sep = "")
        else pkgs[i])
        pkgs <- do.call("c", pkgs)
        bibs <- do.call("c", bibs)
        as.bibkey <- function(x) {
            i <- grep("[.]", x)
            if (length(i) > 0) 
                x[i] <- paste("{", x[i], "}", sep = "")
            x
        }
        bibs <- mapply(function(b, k) {
            b$key <- k
            b
        }, bibs, pkgs, SIMPLIFY = FALSE)
        bibs <- do.call("c", bibs)
        if (verbose) 
            message("Converted ", n.converted, " of ", n.installed, 
                " package citations to BibTeX")
        bibs
    }
    else stop("Invalid argument 'entry': expected a bibentry object or a character vector of package names.")
    if (length(bibs) == 0) {
        if (verbose) 
            message("Empty bibentry list: nothing to be done.")
        return(invisible())
    }
    if (is.null(file)) 
        file <- stdout()
    else if (is.character(file)) {
        if (!grepl("\\.bib$", file)) 
            file <- paste(file, ".bib", sep = "")
    }
    fh <- file(file, open = if (append) 
        "a+"
    else "w+")
    on.exit(if (isOpen(fh)) close(fh))
    if (verbose) 
        message("Writing ", length(bibs), " Bibtex entries ... ", 
            appendLF = FALSE)
    writeLines(toBibtex(bibs), fh)
    if (verbose) 
        message("OK\nResults written to file '", file, "'")
    invisible(bibs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
