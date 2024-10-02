#' @rdname toBiblatex
#' @method toBibtex BibEntry
#' @importFrom utils toBibtex
#' @export
toBibtex.BibEntry <- function(object,
                              note.replace.field = c('urldate', "pubsate",
                                                     "addendum"),
                              extra.fields = NULL, 
                              encoded.names.to.latex = TRUE,
                              ...){
  
  
  object <- .BibEntry_expand_crossrefs(unclass(object), to.bibtex = TRUE)
  if (length(object)) {
    object$.index <- NULL
    rval <- head(unlist(lapply(object, ConvertToBibtex,
                               note.replace.field, extra.fields, 
                               encoded.names.to.latex)), 
                 -1L)
  }
  else rval <- character()
  class(rval) <- "Bibtex"
  rval
}

#' @noRd
ConvertToBibtex <- function(object,
                            note.replace.field,
                            extra.fields,
                            encoded.names.to.latex) {
    object <- unclass(object)[[1L]]
    bibtype <- tolower(attr(object, "bibtype"))
    obj.names <- names(object)
    if (encoded.names.to.latex) {
      if ("author" %in% obj.names)
        object$author <- EncodedNameListToLaTeX(object$author)
      if ("editor" %in% obj.names)
        object$editor <- EncodedNameListToLaTeX(object$editor)
    }
    # see 2.3 Usage Notes p. 28
    if (bibtype == "article" && 'journaltitle' %in% obj.names  &&
        is.null(object$journal))
      object$journal <- object$journaltitle
    
    if ("location" %in% obj.names  && is.null(object$address))
      object$address <- object$location

    object <- ConvertDate(object)
    
    if ("institution" %in% obj.names && bibtype == 'thesis' &&
        is.null(object$school)){
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
      object <- FillNote(object, obj.names, note.replace.field)
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

    bibtype <- ConvertBibtype(bibtype)
    
    rval <- paste0("@", bibtype, "{", attr(object, "key"), ",")
    rval <- c(rval, vapply(names(object)[names(object) %in% c(.Bibtex_fields,
                                                              extra.fields)],
                           function(n) paste0("  ", n, " = {", object[[n]],
                                              "},"), ""), "}", "")
    return(rval)
}

#' Convert Biblatex date to bibtex year/month
#' @noRd
ConvertDate <- function(obj){
    dat <- attr(obj, 'dateobj')
    if (!is.null(dat) && is.null(obj$year)){
      if (is.interval(dat)){
        obj$year <- tolower(year(int_start(dat)))
      }else{
        obj$year <- tolower(year(dat))
      }
    }
    if (!is.null(dat) && attr(dat, "day.mon") > 0 && is.null(obj$month)){
      if (is.interval(dat)){
        obj$month <- tolower(month(int_start(dat), TRUE, TRUE))
      }else{
        obj$month <- tolower(month(dat, TRUE, TRUE))
      }
    }
    obj
}

#' Add field not supported by bibtex to the note field
#' @noRd
FillNote <- function(obj, onames, nrf){
    for (i in seq_along(nrf)){
        if (nrf[i] %in% onames){
          if (nrf[i] == 'urldate'){
            fDate <- try(ProcessDate(obj$urldate, NULL), TRUE)
            if (!is.null(fDate) && !inherits(fDate, 'try-error')){
              obj$note <- paste0('Last visited on ',
                                    MakeBibLaTeX()$DateFormatter(fDate, TRUE))
            }else{
              obj$note <- paste0('Last visited on ', obj$urldate)
            }
          }else{
            obj$note <- obj[[nrf[i]]]
          }
          break
        }
    }
    obj
}

#' @noRd
ConvertBibtype <- function(bibtype){
    types <- tolower(names(BibTeX_entry_field_db))
    if (length(pos <- which(types %in% bibtype)))
        types[pos]
    else
        switch(bibtype, "mvbook" = "Book", "bookinbook" = "InBook",
                      "suppbook" = "InBook", "collection" = "Book",
                      "mvcollection" = "Book",
                      "suppcollection" = "InCollection",
                      "reference" = "Book", "mvreference" = "Book",
                      "inreference" = "InBook", "report" = "TechReport",
                      "proceedings" = "Book", "mvproceedings" = "Book",
                      "periodical" = "Book", "suppperiodical" = "InBook",
                      "patent" = "TechReport", "Misc")
}

.Bibtex_fields <- c("address", "author", "annote", "booktitle", "chapter",
                    "crossref", "edition", "editor", "eprint", "year",
                    "howpublished", "institution", "journal", "month", "key",
                    "note", "primaryclass", "archiveprefix", "doi",
                    "number", "organization", "pages", "publisher", "school",
                    "series", "title", "type", "url", "volume")
