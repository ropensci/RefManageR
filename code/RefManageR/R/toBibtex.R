toBibtex.BibEntry <- function(object, note.replace.field = c('urldate', "pubsate", "addendum"), extra.fields = NULL, ...){
  format_bibentry1 <- function(object) {
    object <- unclass(object)[[1L]]
    bibtype <- attr(object, "bibtype")
    obj.names <- names(object)
#     nl.ind <- which(obj.names %in% .BibEntryNameList)
#     for (i in nl.ind)
#       object[i] <- format_author(object[[i]])
    if ("author" %in% obj.names)
      object$author <- format_author(object$author)
    if ("editor" %in% obj.names)
      object$editor <- format_author(object$editor)  
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
    if (!is.null(dat) && is.null(object$year))
      object$year <- year(attr(object, 'dateobj'))
    if (!is.null(dat) && attr(dat, "day.mon") > 0 && is.null(object$month))
      object$month <- month(dat)
    
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
      bibtype <- ifelse(is.null(object$type) || !object$type == "mathesis", "phdthesis", "mathesis")
    }

    if (!bibtype %in% tolower(names(tools:::BibTeX_entry_field_db)))
      bibtype <- switch(bibtype, "mvbook" = "book", "bookinbook" = "inbook", "suppbook" = "inbook",
                                        "collection" = "book", "mvcollection" = "book", "suppcollection" = "incollection",
                                         "reference" = "book", "mvreference" = "book", "inreference" = "inbook",
                                        "report" = "techreport", "proceedings" = "book", "mvproceedings" = "book",
                                  "periodical" = "book", "suppperiodical" = "inbook", "patent" = "techreport", "misc")
    
    rval <- paste0("@", bibtype, "{", attr(object, "key"), ",")
    rval <- c(rval, sapply(names(object)[names(object) %in% c(.Bibtex_fields, extra.fields)], function(n) paste0("  ", 
                                                             n, " = {", object[[n]], "},")), "}", "")
    return(rval)
  }
  object <- .BibEntry_expand_crossrefs(object)
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