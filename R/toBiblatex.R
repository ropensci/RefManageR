#' Convert BibEntry objects to BibTeX or BibLaTeX
#'
#' toBiblatex converts a BibEntry object to character vectors with BibLaTeX markup.  toBibtex will convert a BibEntry object
#' to character vectors with BibTeX markup, converting some BibLaTeX fields and all entry types that are not supported
#' by BibTeX to ones that are supported.
#' @param object an object of class BibEntry to be converted
#' @param note.replace.field a character vector of BibLaTeX fields.  When converting an entry to BibTeX, the first field in the
#' entry that matches one specified in this vector will be added to the note field, \emph{if} the note field is not already
#' present
#' @param extra.fields character vector; fields that are not supported in standard BibTeX styles are by default dropped
#' in the result return by the toBibtex function.
#' Any fields specified in extra.fields will \emph{not} be dropped if present in an entry.
#' @param encoded.names.to.latex if \code{TRUE} (the default) then name list fields
#' such as \sQuote{author} and \sQuote{editor} will have non-ASCII characters
#' translated to LaTeX escape sequences by \code{\link{latexify}}.
#' @param ... ignored
#' @export
#' @return an object of class \dQuote{Bibtex} - character vectors where each element holds one line of a BibTeX or BibLaTeX file
#' @details toBiblatex converts the BibEntry object to a vector containing the corresponding BibLaTeX file, it ensures the name
#' list fields (e.g. author and editor) are formatted properly to be read by bibtex and biber and otherwise prints all fields
#' as is, thus it is similar to \code{\link{toBibtex}}.
#'
#' toBibtex will attempt to convert BibLaTeX entries to a format that can be read by bibtex.  Any fields not supported by
#' bibtex are dropped unless they are specified in \code{extra.fields}.  The fields below, if they are present, are converted
#' as described and added to a bibtex supported field, unless that field is already present.
#' \itemize{
#' \item date - The \code{date} field, if present will be truncated
#' to a year and added to the \code{year} field, if it is not already present. If a month is specified with the date, it will
#' be added to the \code{month} field.
#' \item journaltitle - Will be changed to journal, if it is not already present
#' \item location - Will be changed to address
#' \item institution - Converted to \code{school} for thesis entries
#' \item sortkey - Converted to \code{key}
#' \item maintitle - Converted to \code{series}
#' \item issuetitle - Converted to \code{booktitle}
#' \item eventtitle - Converted to \code{booktitle}
#' \item eprinttype - Converted to \code{archiveprefix} (for arXiv references)
#' \item eprintclass - Converted to \code{primaryclass} (for arXiv references)
#' }
#'
#' If no \code{note} field is present, the note.replace.field can be used to specified BibLaTeX fields that can be looked for
#' and added to the note field if they are present.
#'
#' BibLaTeX entry types that are not supported by bibtex are converted by toBibtex as follows
#' "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
#' \itemize{
#' \item MvBook,Collection,MvCollection,Reference,MvReference,Proceedings,MvProceedings,Periodical - to Book
#' \item BookInBook,SuppBook,InReference,SuppPeriodical - to InBook
#' \item report,patent - to TechReport
#' \item SuppCollection - to InCollection
#' \item thesis - to MastersThesis if \code{type = mathesis}, else to PhdThesis
#' \item \emph{rest} - to Misc
#' }
#' @seealso \code{\link{toBibtex}}, \code{\link{BibEntry}}, \code{\link{print.BibEntry}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @importFrom tools parseLatex deparseLatex latexToUtf8
#' @keywords database IO utilities
#' @aliases toBibtex.BibEntry toBibtex
#' @examples
#' if (requireNamespace("bibtex")) {
#'     file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#'     bib <- suppressMessages(ReadBib(file.name))
#'     toBiblatex(bib[70:72])
#'     toBibtex(bib[70:72])
#' }
toBiblatex <- function(object, encoded.names.to.latex = TRUE, ...){
    format_bibentry1 <- function(object) {
      object <- unclass(object)[[1L]]
      rval <- paste0("@", attr(object, "bibtype"), "{", attr(object,
          "key"), ",")
      if (encoded.names.to.latex) {
        nl.ind <- which(names(object) %in% .BibEntryNameList)
        for (i in nl.ind)
          object[i] <- EncodedNameListToLaTeX(object[[i]])
      }
      rval <- c(rval, vapply(names(object), function(n) paste0("  ",
          n, " = {", object[[n]], "},"), ""), "}", "")
      return(rval)
    }
    if (length(object)) {
        object$.index <- NULL
        rval <- head(unlist(lapply(object, format_bibentry1)),
            -1L)
    }
    else rval <- character()
    class(rval) <- "Bibtex"
    rval
}

#' Wrapper for dplr::latexify that returns original
#' text if translation to LaTeX fails
#' @importFrom dplR latexify
#' @noRd
#' @seealso  \url{https://github.com/ropensci/RefManageR/issues/106}
EncodedNameListToLaTeX <- function(name.list, encoding = "UTF-8")
{
  formatted.text <- format_author(name.list)
  out <- latexify(formatted.text, encoding)
  if (grepl("^[{]?[?]", out))
    return(formatted.text)
  return(out)
}
