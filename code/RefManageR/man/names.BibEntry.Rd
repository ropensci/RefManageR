\name{names.BibEntry}
\alias{names.BibEntry}
\alias{names<-.BibEntry}
\title{Names (keys) of a BibEntry object}
\usage{
\method{names}{BibEntry}(x)

\method{names}{BibEntry}(x) <- value
}
\arguments{
  \item{x}{an object of class BibEntry}

  \item{value}{character vector of new key values to
  replace into \code{x}}
}
\value{
\code{names} - character vector of the keys of the BibEntry
object.

\code{names<-} the updated BibEntry object.
}
\description{
Functions to get and set the keys of an object of class
BibEntry
}
\examples{
bib <- ReadBib(system.file("Bib", "test.bib", package = "RefManageR"))
names(bib)
names(bib)[1] <- 'newkey'
}
\author{
McLean, M. W. \email{mathew.w.mclean@gmail.com}
}
\keyword{attribute}

