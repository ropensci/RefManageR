\name{BibOptions}
\alias{BibOptions}
\title{Set options/hooks for RefManageR}
\usage{
BibOptions(..., restore.defaults = FALSE)
}
\arguments{
  \item{...}{- a character vector or strings specifying
  option names to access; or to set options values, a named
  list or vector of option values or options specified in
  name=value pairs.}

  \item{restore.defaults}{- logical; if TRUE, \code{...}'s
  are ignored and all package options are restored to their
  defaults.}
}
\value{
if a vector of option names is supplied, the current value
of the requested options, or if \code{...} is missing, all
current option values; otherwise, when setting options the
old values of the changed options are (invisibly) returned
as a list.
}
\description{
This function is used to access and set package options for
RefManageR, similar to \code{\link{options}}. The options
are listed in the details
}
\details{
The following are valid package options \enumerate{ \item
match.author - string; for \code{\link{SearchBib}} and the
operators \code{\link{[.BibEntry}} and
\code{\link{[<-.BibEntry}}, controls how name list fields
(author, editor, translator, etc.) are matched when
searching for names. \dQuote{family.with.initials} require
family names and given name initials to match,
\dQuote{exact} requires names to match exactly, and any
other value results in only family names being compared
(the default). \item match.date - string; for
\code{\link{SearchBib}} and the operators
\code{\link{[.BibEntry}} and \code{\link{[<-.BibEntry}},
controls how date fields are matched when searching.  If
\dQuote{year.only} (the default), only years are checked
for equality when comparing dates, otherwise months and
days will also be compared, if they are available. \item
return.ind - logical; if \code{TRUE} the return value of
\code{\link{SearchBib}} and the operators
\code{\link{[.BibEntry}} and \code{\link{[<-.BibEntry}},
will be the indices of any matches; otherwise, a
\code{BibEntry} object is returned. \item
merge.fields.to.check - character vector; for
\code{\link{merge.BibEntry}} and the operator
\code{\link{+.BibEntry}}, the fields that should be checked
when comparing entries for equality when merging BibEntry
objects.  Specifying \dQuote{all} results in all fields be
checked with \code{\link{duplicated}}.  The default is
\dQuote{key} to only check for duplicated keys. \item
bib.style - string; Biblatex bibliography style to use when
printing and formatting a BibEntry object.  Possible values
are \dQuote{numeric} (default), \dQuote{authoryear},
\dQuote{authortitle}, \dQuote{alphabetic}, \dQuote{draft};
see \code{\link{print.BibEntry}}. \item first.inits -
logical; if \code{TRUE}, only given name initials are
displayed when printing; otherwise, full names are used.
\item dashed - logical; if \code{TRUE} and \code{bib.style
= "authoryear"} or \code{bib.style = "authortitle"},
recurring author and editor names are replaced with
\dQuote{---} when printing. \item sorting - string;
controls how BibEntry objects are sorted.  Possible values
are \dQuote{nty}, \dQuote{nyt}, \dQuote{nyvt},
\dQuote{anyt}, \dQuote{anyvt}, \dQuote{ynt}, \dQuote{ydnt},
\dQuote{none}, \dQuote{debug}; see
\code{\link{sort.BibEntry}} \item check.entries - string or
\code{FALSE}; if \code{FALSE} entries are not checked it
ensure that they have all the required fields for the type
of entry; if \dQuote{warn} then entries are checked, but
only a warning is issued and the entry is processed anyway;
otherwise an error is produced if an entry does not have
the required fields (default).  Note that the majority of
fields listed as required for a particular entry type in
the Biblatex manual are not actually required for Biblatex
to produce an entry. \item use.regex - logical; if
\code{TRUE}, regular expressions are used when searching
non-date fields; otherwise, exact matching is used. \item
ignore.case - logical; if \code{TRUE}, case is ignored when
searching. \item max.names - numeric; maximum number of
names to display before using \dQuote{et al.} when
formatting and printing name list fields.  This is also the
minimum number of names that will be displayed if
\dQuote{et al.} is used (minnames package option in
Biblatex) }
}
\note{
if \code{...} is missing and \code{restore.defaults =
FALSE}, all options and their current values will be
returned as a list.
}
\examples{
BibOptions()
BibOptions("first.inits", "bib.style")

oldopts <- BibOptions(first.inits = FALSE, bib.style = "authoryear")
oldopts
BibOptions(oldopts)

BibOptions(restore.defaults = TRUE)
}
\seealso{
\code{\link{print.BibEntry}}, \code{\link{BibEntry}},
\code{\link{options}}
}

