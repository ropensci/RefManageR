\name{format.Bibentry}
\alias{format.Bibentry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
format.Bibentry(x, style = "text", .bibstyle = NULL, citation.bibtex.max = getOption("citation.bibtex.max", 1), sort = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{style}{
%%     ~~Describe \code{style} here~~
}
  \item{.bibstyle}{
%%     ~~Describe \code{.bibstyle} here~~
}
  \item{citation.bibtex.max}{
%%     ~~Describe \code{citation.bibtex.max} here~~
}
  \item{sort}{
%%     ~~Describe \code{sort} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, style = "text", .bibstyle = NULL, citation.bibtex.max = getOption("citation.bibtex.max", 
    1), sort = FALSE, ...) 
{
    style <- .bibentry_match_format_style(style)
    if (sort) 
        x <- sort(x, .bibstyle = .bibstyle)
    x$.index <- as.list(seq_along(x))
    .format_bibentry_via_Rd <- function(f) {
        out <- file()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({
            tools::Rd2txt_options(saveopt)
            close(out)
        })
        sapply(.bibentry_expand_crossrefs(x), function(y) {
            rd <- tools::toRd(y, style = .bibstyle)
            con <- textConnection(rd)
            on.exit(close(con))
            f(con, fragment = TRUE, out = out, ...)
            paste(readLines(out), collapse = "\n")
        })
    }
    .format_bibentry_as_citation <- function(x) {
        bibtex <- length(x) <= citation.bibtex.max
        c(paste(strwrap(attr(x, "mheader")), collapse = "\n"), 
            unlist(lapply(x, function(y) {
                paste(c(if (!is.null(y$header)) c(strwrap(y$header), 
                  ""), if (!is.null(y$textVersion)) {
                  strwrap(y$textVersion, prefix = "  ")
                } else {
                  format(y)
                }, if (bibtex) {
                  c(gettext("\nA BibTeX entry for LaTeX users is\n"), 
                    paste0("  ", unclass(toBibtex(y))))
                }, if (!is.null(y$footer)) c("", strwrap(y$footer))), 
                  collapse = "\n")
            })), paste(strwrap(attr(x, "mfooter")), collapse = "\n"))
    }
    out <- switch(style, text = .format_bibentry_via_Rd(tools::Rd2txt), 
        html = .format_bibentry_via_Rd(tools::Rd2HTML), latex = .format_bibentry_via_Rd(tools::Rd2latex), 
        Bibtex = {
            unlist(lapply(x, function(y) paste(toBibtex(y), collapse = "\n")))
        }, textVersion = {
            out <- lapply(unclass(x), attr, "textVersion")
            out[!sapply(out, length)] <- ""
            unlist(out)
        }, citation = .format_bibentry_as_citation(x), R = .format_bibentry_as_R_code(x, 
            ...))
    as.character(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
