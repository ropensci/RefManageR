\name{[.BibEntry}
\alias{SearchBib}
\alias{[.BibEntry}
\title{Search BibEntry objects by field}
\usage{
\method{[}{BibEntry}(x, i, j, ..., drop = FALSE)

SearchBib(x, .opts = list(), ...)
}
\arguments{
  \item{x}{an object of class BibEntry}

  \item{i}{A named list or character vector of search terms
  with names corresponding to the field to search for the
  search term.  Alternatively, a vector of entry key values
  or numeric or logical indices specifying which bibentries
  to exctract.}

  \item{j}{A named list or character vector, as \code{i}.
  Entries matching the search specified by i \emph{OR}
  matching the query specified by \code{j} will be return}

  \item{...}{arguments in the form \code{bib.field =
  search.term}, or as \code{j} list\emph{s} or character
  vector\emph{s} for additional searches.  For
  \code{SearchBib}, can alternatively have same form as
  \code{i}.}

  \item{drop}{logical, should attributes besides class be
  dropped from result?}

  \item{.opts}{list of search options with \code{name =
  value} entries.  Any option described in
  \code{\link{BibOptions}} is valid, with the following
  being the most relevant ones \itemize{ \item
  \code{use.regex} - logical; are the search terms regular
  expressions or should exact matching be used? \item
  \code{ignore.case} - logical; should case be ignored when
  comparing strings? \item \code{match.date} - how should
  the date fields date, urldate, eventdate, and origdate.
  Default is \dQuote{year.only}, so that months and days in
  dates are ignored when comparing.  Currently, specifying
  any other value results the full date being used.  See
  the Note section. \item \code{match.author} - character
  string; how should name fields be searched? If
  \dQuote{family.only}, only family names are compared; if
  \dQuote{family.with.initials}, family name and given name
  initials are used; if \dQuote{exact}, full names are
  used. \item \code{return.ind} - logical; if TRUE the
  returned object is numeric indices of match locations;
  otherwise, a BibEntry object is returned }}
}
\value{
an object of class BibEntry (the results of the
search/indexing), \emph{or} if
\code{BibOptions()$return.ind=TRUE}, the indices in
\code{x} that match the search terms.
}
\description{
Allows for searching and indexing a BibEntry object by
fields, including names and dates.  The extraction operator
and the SearchBib function simplying provide different
interfaces to the same search functionality.
}
\note{
The arguments to the SearchBib function that control
certain search features can also be changed for the
extraction operator by changing the corresponding option in
the .BibOptions object; see \code{\link{BibOptions}}.
}
\examples{
file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- suppressMessages(ReadBib(file.name))

## author search, default is to use family names only for matching
bib[author = "aristotle"]

## Aristotle references before 1925
bib[author="aristotle", date = "/1925"]

## Aristotle references before 1925 *OR* references with editor Westfahl
bib[list(author="aristotle", date = "/1925"),list(editor = "westfahl")]

## Change some searching and printing options and search for author
old.opts <- BibOptions(bib.style = "authoryear", match.author = "exact",
  max.names = 99, first.inits = FALSE)
bib[author="Mart\\u00edn, Jacinto and S\\u00e1nchez, Alberto"]
BibOptions(old.opts)  ## reset options

## Some works of Raymond J. Carroll's
file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
bib <- ReadBib(file.name)
length(bib)

## index by key
bib[c("chen2013using", "carroll1978distributions")]

## Papers with someone with family name Wang
length(SearchBib(bib, author='Wang', .opts = list(match.author = "family")))

## Papers with Wang, N.
length(SearchBib(bib, author='Wang, N.', .opts = list(match.author = "family.with.initials")))

## tech reports with Ruppert
length(bib[author='ruppert',bibtype="report"])

##Carroll and Ruppert tech reports at UNC
length(bib[author='ruppert',bibtype="report",institution="north carolina"])

## Carroll and Ruppert papers since leaving UNC
length(SearchBib(bib, author='ruppert', date="1987-07/",
   .opts = list(match.date = "exact")))

## Carroll and Ruppert papers NOT in the 1990's
length(SearchBib(bib, author='ruppert', date = "!1990/1999"))
identical(SearchBib(bib, author='ruppert', date = "!1990/1999"),
  SearchBib(bib, author='ruppert', year = "!1990/1999"))
table(unlist(SearchBib(bib, author='ruppert', date="!1990/1999")$year))

## Carroll + Ruppert + Simpson
length(bib[author="Carroll, R. J. and Simpson, D. G. and Ruppert, D."])

## Carroll + Ruppert OR Carroll + Simpson
length(bib[author=c("Carroll, R. J. and Ruppert, D.", "Carroll, R. J. and Simpson, D. G.")])

## Carroll + Ruppert tech reports at UNC "OR" Carroll and Ruppert JASA papers
length(bib[list(author='ruppert',bibtype="report",institution="north carolina"),
  list(author="ruppert",journal="journal of the american statistical association")])
}
\seealso{
Other operators: \code{\link{$.BibEntry}};
\code{\link{$<-.BibEntry}}; \code{\link{+.BibEntry}},
\code{\link{+.BibEntry}}, \code{\link{merge.BibEntry}};
\code{\link{[<-.BibEntry}}; \code{\link{[[.BibEntry}};
\code{\link{[[<-.BibEntry}}; \code{\link{c.BibEntry}}
}
\keyword{database}
\keyword{list}
\keyword{manip}

