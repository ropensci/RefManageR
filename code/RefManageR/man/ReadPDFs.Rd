\name{ReadPDFs}
\alias{ReadPDFs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Create bibliographic information from PDF Metadata.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function creates bibliographic information by reading the Metadata and text of PDFs stored in a user specified directory using Poppler (\url{http://poppler.freedesktop.org/}).  IF requested, the function first searches for DOIs and downloads \code{BibTeX} entries from \code{\link{ReadCrossRef}} if DOIs are found.  If this is not requrested or a DOI is not found for an entry, an attempt is made to build a BibTeX entry from the metadata and text.
}
\usage{
ReadPDFs(path, encoding = "UTF-8", recursive = TRUE, use.crossref = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
  character; path to directory containing pdfs or filename of one pdf. \code{normalizePath} is used on the specified path
}
  \item{encoding}{
%%     ~~Describe \code{encoding} here~~
character; text encoding to use for reading pdf and creating BibEntry object. Available encodings for Poppler can be found using \code{system(\dQuote{pdfinfo -listenc})}.  The encoding must also be listed in \code{iconvlist()}.
}
  \item{recursive}{
%%     ~~Describe \code{recursive} here~~
logical; same as \code{list.files}.  Should pdfs in subdirectories of path be used?
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function requires that the \code{pdfinfo} utility from Poppler PDF \url{http://poppler.freedesktop.org/} be installed.

This function is only likely to be successful with published journal articles, and may not work with certain journals.  This function will create only \code{Article} or \code{Misc} \code{BibTeX} entries.  The function requires that a pdf have at least (non-empty string) \sQuote{Author}, \sQuote{Title} and \sQuote{CreationDate} or \sQuote{ModDate} fields in the metadata.  If an additional field, \sQuote{Subject}, is not available, a \code{Misc} \code{BibTeX} entry will be created.  If \sQuote{Subject} is available, then an \code{Article} entry will be created.  The \sQuote{Subject} data is processed assuming comma separated entries for journal, volume, number, pages, and note; with only journal being assumed/required.  For example, \dQuote{Journal Name, Vol.123, No.2, 12-43, the note}.

The absolute path to each file will be stored in the bib entry in a field called \sQuote{file}, which is recognized by \code{BibLaTeX} (though not printed by any standard style) and can be used by the \code{\link{open.BibEntry}} function to open the PDF in the default viewer.

If the keywords metadata field is available, it will be added to the bib entry in a field \sQuote{keywords}, which is recognized by \code{BibLaTeX}.
}
\value{
An object of class BibEntry.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
\url{http://poppler.freedesktop.org/}
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
BibEntry, \code{tm} package
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{database}
