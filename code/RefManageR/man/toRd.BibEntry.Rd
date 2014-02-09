\name{toRd.BibEntry}
\alias{toRd.BibEntry}
\title{Convert BibEntry object to a fragment of Rd code.}
\usage{
\method{toRd}{BibEntry}(obj, ...)
}
\arguments{
  \item{obj}{- An object of class BibEntry}

  \item{style}{- The bibstyle to be used for converting
  \code{obj}; see \code{\link{print.BibEntry}}}

  \item{.sorting}{- the BibLaTeX sorting method to use; see
  \code{\link{sort.BibEntry}}}

  \item{...}{- ignored}
}
\value{
Returns a character vector containing a fragment of Rd code
that could be parsed and rendered.
}
\description{
Renders references in a BibEntry object as a fragment of Rd
code, which can then be rendered into text, HTML, or LaTex.
}
\seealso{
\code{\link{print.BibEntry}}, \code{\link{sort.BibEntry}},
\code{\link{BibEntry}}, \code{\link{bibstyle}}
}
\keyword{internal}

