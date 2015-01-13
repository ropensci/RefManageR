#' @rdname toBiblatex
#' @method toBibtex BibEntry
#' @importFrom utils toBibtex
#' @export
toBibtex.BibEntry <- function(object, note.replace.field = c('urldate', "pubsate", "addendum"), extra.fields = NULL, ...){
  format_bibentry1 <- function(object){
    object <- unclass(object)[[1L]]
    bibtype <- tolower(attr(object, "bibtype"))
    obj.names <- names(object)
    if ("author" %in% obj.names)
      object$author <- encoded_text_to_latex(format_author(object$author), "UTF-8")
    if ("editor" %in% obj.names)
      object$editor <- encoded_text_to_latex(format_author(object$editor), "UTF-8")
    # see 2.3 Usage Notes p. 28
    
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
    
    if ("institution" %in% obj.names && bibtype == 'thesis' && is.null(object$school)){
      object$school <- object$institution
      object$institution <- NULL 
    }
    if ("eprinttype" %in% obj.names && is.null(object$archiveprefix))
      object$archiveprefix <- object$eprinttype
    if ("eprintclass" %in% obj.names && is.null(object$primaryclass))
      object$primaryclass <- object$eprintclass
    if ("sortkey" %in% obj.names && !"key" %in% obj.names)
      object$key <- object$sortkey
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
              object$note <- paste0('Last visited on ', MakeBibLaTeX()$DateFormatter(fDate, TRUE))  
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
    
    if (bibtype == "thesis" && length(object$type)){
      bibtype <- switch(object$type, mathesis = { 
        object$type <- NULL
        "mastersthesis" 
      }, phdthesis = { 
        object$type  <- NULL
        "phdthesis"
      }, "phdthesis")
    }
    
    pos <- match(bibtype, tolower(names(BibTeX_entry_field_db)))
    if (is.na(pos)){
      bibtype <- switch(bibtype, "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
                        "collection" = "Book", "mvcollection" = "Book", "suppcollection" = "InCollection",
                        "reference" = "Book", "mvreference" = "Book", "inreference" = "InBook",
                        "report" = "TechReport", "proceedings" = "Book", "mvproceedings" = "Book",
                        "periodical" = "Book", "suppperiodical" = "InBook", "patent" = "TechReport", "Misc")
    }else{
      bibtype <- names(BibTeX_entry_field_db)[pos]
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