#' Convert BibEntry objects to BibTeX or BibLaTeX
#' 
#' toBiblatex converts a BibEntry object to character vectors with BibLaTeX markup.  toBibtex will convert a BibEntry object
#' to character vectors with BibTeX markup, converting some BibLaTeX fields and all entry types that are not supported 
#' by BibTeX to ones that are supported.  
#' @param object - an object of class BibEntry to be converted
#' @param note.replace.field - a character vector of BibLaTeX fields.  When converting an entry to BibTeX, the first field in the
#' entry that matches one specified in this vector will be added to the note field, \emph{if} the note field is not already 
#' present
#' @param extra.fields - character vector; fields that are not supported in standard BibTeX styles are by default dropped 
#' in the result return by the toBibtex function.  
#' Any fields specified in extra.fields will \emph{not} be dropped if present in an entry.
#' @S3method toBibtex BibEntry
#' @aliases toBiblatex
#' @details toBiblatex converts the BibEntry object to a vector containing the corresponding BibLaTeX file, it ensures the name
#' list fields (e.g. author and editor) are formatted properly to be read by bibtex and biber and otherwise prints all fields
#' as is, thus it is very similar \code{\link{to toBibtex.bibentry}}.
#' 
#' toBibtex will attempt to convert BibLaTeX entries to a format that can be read by bibtex.  Any fields not supported by 
#' bibtex are dropped unless they are specified in \code{extra.fields}.  The fields below, if they are present, are converted
#' as described and added to a bibtex supported field, unless that field is already present.
#' \item{date}{The \code{date} field, if present will be truncated 
#' to a year and added to the \code{year} field, if it is not already present. If a month is specified with the date, it will
#' be added to the \code{month} field.}
#' \item{journaltitle}{Will be changed to journal, if it is not already present}
#' \item{location}{Will be changed to address}
#' \item{institution}{Converted to \code{school} for thesis entries}
#' \item{sortkey}{Converted to \code{key}}
#' \item{annotation}{Converted to \field{annote}}
#' \item{maintitle}{Converted to \field{series}}
#' \item{issuetitle}{Converted to \field{booktitle}}
#' \item{eventtitle}{Converted to \field{booktitle}}
#' \item{eprinttype}{Converted to \field{archiveprefix} (for arXiv references)}
#' \item{eprintclass}{Converted to \field{primaryclass} (for arXiv references)}
#' If no \code{note} field is present, the note.replace.field can be used to specified BibLaTeX fields that can be looked for
#' and added to the note field if they are present.
#' 
#' BibLaTeX entry types that are not supported by bibtex are converted by toBibtex as follows
#' "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
#' \item{MvBook,Collection,MvCollection,Reference,MvReference,Proceedings,MvProceedings,Periodical}{to Book}
#' \item{BookInBook,SuppBook,InReference,SuppPeriodical}{to InBook}
#' \item{report,patent}{to TechReport}
#' \item{SuppCollection}{to InCollection}
#' \item{thesis}{to MastersThesis if \code{type = mathesis}, else to PhdThesis}
#' \item{\emph{rest}}{to Misc}
#' @seealso \code{\link{BibEntry}}, \code{\link{print.BibEntry}}
#' @author McLean, M. W. \email{mathew.w.mclean@gmail.com}
#' @value an object of class \dQuote{Bibtex} - character vectors where each element holds one line of a BibTeX or BibLaTeX file
#' @importFrom tools encoded_text_to_latex parseLatex deparseLatex latexToUtf8
toBibtex.BibEntry <- function(object, note.replace.field = c('urldate', "pubsate", "addendum"), extra.fields = NULL, ...){
  format_bibentry1 <- function(object) {
    object <- unclass(object)[[1L]]
    bibtype <- tolower(attr(object, "bibtype"))
    obj.names <- names(object)
#     nl.ind <- which(obj.names %in% .BibEntryNameList)
#     for (i in nl.ind)
#       object[i] <- format_author(object[[i]])
    if ("author" %in% obj.names)
      object$author <- encoded_text_to_latex(format_author(object$author), "UTF-8")
    if ("editor" %in% obj.names)
      object$editor <- encoded_text_to_latex(format_author(object$editor), "UTF-8")
    # see 2.3 Usage Notes p. 28
    # DONE don't need to format non-author/editor Name lists
    # map entry types
    # map extra field to empty note or annote
    # handle crossref
    # does bibtex automatically parent title to child booktitle?
    # electronic and www entries?
    # drop extra fields
    
    if (bibtype == "article" && 'journaltitle' %in% obj.names  && is.null(object$journal))
      object$journal <- object$journaltitle

    if ("location" %in% obj.names  && is.null(object$address))
      object$address <- object$location
    
    dat <- attr(object, 'dateobj')
    if (!is.null(dat) && is.null(object$year)){
      if (is.interval(dat)){
        object$year <- tolower(year(int_start(dat)))
      }else{
        object$year <- tolower(year(dat))
      }
    }
    if (!is.null(dat) && attr(dat, "day.mon") > 0 && is.null(object$month)){
      if (is.interval(dat)){
        object$month <- tolower(month(int_start(dat), TRUE, TRUE))
      }else{
        object$month <- tolower(month(dat, TRUE, TRUE))
      }
    }
    
    if ("institution" %in% obj.names && bibtype == 'thesis' && is.null(object$school))
      object$school <- object$institution
    if ("eprinttype" %in% obj.names && is.null(object$archiveprefix))
      object$archiveprefix <- object$eprinttype
    if ("eprintclass" %in% obj.names && is.null(object$primaryclass))
      object$primaryclass <- object$eprintclass
    if ("sortkey" %in% obj.names && !"key" %in% obj.names)
      object$key <- object$sortkey
    if ("annotation" %in% obj.names && !"annote" %in% obj.names)
      object$annote <- object$annotation
    if ("maintitle" %in% obj.names && !"series" %in% obj.names)
      object$series <- object$maintitle
    if ("issuetitle" %in% obj.names && !"booktitle" %in% obj.names)
      object$booktitle <- object$issuetitle
    if ("eventtitle" %in% obj.names && !"booktitle" %in% obj.names)
      object$booktitle <- object$eventtitle

    # fill empty note with urldate or pubstate
    if (!"note" %in% obj.names && length(note.replace.field)){
      for (i in seq_along(note.replace.field)){
        if (note.replace.field[i] %in% obj.names){
          if (note.replace.field[i] == 'urldate'){
            fDate <- try(ProcessDate(object$urldate, NULL), TRUE)
            if (!is.null(fDate) && !inherits(fDate, 'try-error')){
              object$note <- paste0('Last visited on ', tools::bibstyle('BibLaTeX')$DateFormatter(fDate, TRUE))  
            }else{
              object$note <- paste0('Last visited on ', object$urldate)
            }
          }else{
            object$note <- object[[note.replace.field[i]]]
          }
          break
        }
      }
    }
    
    if (bibtype == "thesis"){
      bibtype <- ifelse(is.null(object$type) || !object$type == "mathesis", "PhdThesis", "MastersThesis")
    }
    
    pos <- match(bibtype, tolower(names(tools:::BibTeX_entry_field_db)))
    if (is.na(pos)){
      bibtype <- switch(bibtype, "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
                                        "collection" = "Book", "mvcollection" = "Book", "suppcollection" = "InCollection",
                                         "reference" = "Book", "mvreference" = "Book", "inreference" = "InBook",
                                        "report" = "TechReport", "proceedings" = "Book", "mvproceedings" = "Book",
                                  "periodical" = "Book", "suppperiodical" = "InBook", "patent" = "TechReport", "Misc")
    }else{
      bibtype <- names(tools:::BibTeX_entry_field_db)[pos]
    }
    
    rval <- paste0("@", bibtype, "{", attr(object, "key"), ",")
    rval <- c(rval, sapply(names(object)[names(object) %in% c(.Bibtex_fields, extra.fields)], function(n) paste0("  ", 
                                                             n, " = {", object[[n]], "},")), "}", "")
    return(rval)
  }
  
  object <- .BibEntry_expand_crossrefs(unclass(object), to.bibtex = TRUE)
  if (length(object)) {
    object$.index <- NULL
    rval <- head(unlist(lapply(object, format_bibentry1)), 
                 -1L)
  }
  else rval <- character()
  class(rval) <- "Bibtex"
  rval
}

.Bibtex_fields <- c("address", "author", "annote", "booktitle", "chapter", "crossref", "edition", "editor", "eprint", "year",
                    "howpublished", "institution", "journal", "month", "key", "note", "primaryclass", "archiveprefix", "doi",
                    "number", "organization", "pages", "publisher", "school", "series", "title", "type", "url", "volume")