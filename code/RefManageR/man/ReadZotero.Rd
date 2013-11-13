\name{ReadZotero}
\alias{ReadZotero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get Bibliography Information From a Zotero Library
}
\description{
Queries an existing Zotero library to import bibliographic information.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadZotero(user, .params, file = "tempbib.bib", curl = getCurlHandler())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{
%%     ~~Describe \code{user} here~~
Zotero userID for use in calls to the Zotero API.  This is not the same as your Zotero username.  The userID for accessing user-owned libraries can be found at \link{https://www.zotero.org/settings/keys}.
}
  \item{.params}{
%%     ~~Describe \code{.params} here~~
A \emph{named} list of parameters to use in requests to the Zotero API with possible values
  \itemize{
    \item q - Search string to use to search the library
    \item qmode - Search mode. Default is "titleCreatorYear".  Use "everything" to include full-text content in search.
    \item key - API key.  This must be specified to access non-public libraries.
    \item collection - name of a specific collection within the library to search
    \item itemType - type of entry to search for; e.g., "book" or "journalArticle"
    \item tag - name of tag to search for in library
    \item limit - maximum number of entries to return
    \item start - index of first entry to return
  }
}
  \item{file}{
%%     ~~Describe \code{file} here~~
File name where the BibTeX data returned by Zotero will be temporarily written.
}
  \item{curl}{
%%     ~~Describe \code{curl} here~~
the \code{CURLHandle} object created earlier if one is reusing these objects. Otherwise, a new one is generated and discarded.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class BibEntry
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
McLean, M. W.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{BibEntry}}, \code{WriteZotero}, \code{\link{getForm}} in package \code{RCurl}
}
\examples{
## first two entries in library with bayesian in title
ReadZotero(user='1648676', .params=list(q='bayesian', key='7lhgvcwVq60CDi7E68FyE3br', limit=2))

## Search specific collection
## collection key can be found by reading uri when collection is selected in Zotero
ReadZotero(user='1648676', .params=list(q='yu', key='7lhgvcwVq60CDi7E68FyE3br', collection='3STEQRNU'))

## Search by tag
## Notice issue with how Zotero uses Techreport entry for arXiv manuscripts
ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Interfaces to databases }
