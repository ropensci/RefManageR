\name{relist.BibEntry}
\alias{relist.BibEntry}
\alias{unlist.BibEntry}
\title{Flatten and unflatten BibEntry objects}
\usage{
\method{unlist}{BibEntry}(x, recursive = FALSE, use.names = TRUE)

relist.BibEntry(flesh, skeleton = NULL)
}
\arguments{
  \item{flesh}{list; an \code{unlist}ed BibEntry object}

  \item{skeleton}{currently ignored}

  \item{x}{a BibEntry object to flatten}

  \item{recursive}{ignored.}

  \item{use.names}{ignored.}
}
\value{
\code{relist.BibEntry} - an object of class BibEntry

For \code{unlist}, a list with bib entries collapsed into a
single list.
}
\description{
\code{relist.BibEntry} unflattens a BibEntry object that
has been flattened with \code{unlist}.

\code{unlist} flattens a BibEntry object to a single list
where every field (including \code{bibtype} and \code{key})
of every entry is a separate element in the list.
}
\details{
\code{relist.BibEntry} is only intended for use with
\code{unlist}ed BibEntry objects.
}
\note{
The names of the list elements from an unlisted BibEntry
object will not be unique.  To do this see
\code{\link{make.unique}}.
}
\examples{
bib <- list(c(bibtype = "article", key = "mclean2014a", title = "My New Article",
  author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-01"),
  c(bibtype = "article", key = "mclean2014b", title = "My Newer Article",
  author = "Mathew W. McLean", journaltitle = "The Journal", date = "2014-02"))
bib <- as.BibEntry(bib)
unlist(bib)
relist.BibEntry(unlist(bib))
}
\seealso{
\code{\link{as.BibEntry}}
}
\keyword{database}
\keyword{list}
\keyword{manip}

