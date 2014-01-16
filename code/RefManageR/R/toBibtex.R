toBibtex.BibEntry <- function(object, note.replace.field = c('urldate', "pubsate", "addendum"), extra.fields = NULL, ...){
  format_bibentry1 <- function(object) {
    object <- unclass(object)[[1L]]
    bibtype <- tolower(attr(object, "bibtype"))
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
  
  object <- .BibEntryExpandCrossrefsTB(object)
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

.BibEntryExpandCrossrefsTB <- function (x) {
  # browser()
  if (!length(x))
    return(NULL)
  x <- unclass(x)
  xrefs <- lapply(x, '[[', "xdata")
  px <- which(vapply(xrefs, length, 0L) > 0L)
  if (length(px)){
    xk <- match(unlist(xrefs[px]), .BibEntry_get_key(x)) 
    ok <- !is.na(xk)
    x[px[ok]] <- Map(function(chi, xdat) {
      add <- setdiff(names(xdat), names(chi))
      # titleaddon and subtitle in parent have special fields for child
      # ensure child with no subtitle, titleaddon don't inherit them incorrectly
      chi[add] <- xdat[add]      
      if (is.null(attr(chi, 'dateobj')))
        attr(chi, 'dateobj') <- attr(xdat, 'dateobj')
      chi
    }, x[px[ok]], x[xk[ok]])
  }
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .BibEntry_get_key(x))
    ok <- !is.na(pk)
    x[pc[ok]] <- lapply(x[pc[ok]], function(bib){
      if (attr(bib, 'bibtype') %in% c("InBook", "InCollection", "InProceedings") && is.null(bib$subtitle))
        bib$subtitle <- ''
      bib
    })
    x[pk[ok]] <- lapply(x[pk[ok]], function(bib){
      if (attr(bib, 'bibtype') %in% c("Book", "Proceedings") && is.null(bib$subtitle))
        bib$booktitle <- bib$title
      bib
    })
    status <- lapply(x[pc], function(e) tryCatch(utils:::.bibentry_check_bibentry1(e, 
                                                                     FALSE), error = identity))
    bad <- which(sapply(status, inherits, "error"))
    if (length(bad)) {
      for (b in bad) {
        warning(gettextf("Dropping invalid entry %d:\n%s", 
                         pc[b], conditionMessage(status[[b]])))
      }
      x[pc[bad]] <- NULL
    }
  }
  types <- sapply(x, attr, "bibtype")
  class(x) <- c("BibEntry", "bibentry")
  
  x[!types %in% c('Set', 'Xdata')]
}