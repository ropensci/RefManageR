# Mathew McLean
# October 25, 2013
# Make bibentry class compatible with BibLaTeX

################################
# to be imported in package: 
# utils:::.bibentry_match_format_style(style)
# utils:::bibentry_attribute_names # <- c("bibtype", "textVersion", "header", "footer", "key")
# utils:::bibentry_list_attribute_names
# utils:::citation.bibtex.max
# utils:::.bibentry_get_key

#source('~/biblatex/code/BibLaTeX_entry_field_db.R')
library(bibtex)
#oldbibentry <- bibentry
# old.bibentry_Check_bibentry1 <- utils:::.bibentry_check_bibentry1

# setClass('bibentry')
# setClass('BibEntry', contains='bibentry')
# setGeneric('table')
# setGeneric('search')

# 
# setMethod("[",
#           "BibEntry",
#           function(x, ..., drop=TRUE){
#             if(!length(x))
#               return(x)
#             
#             dots <- list(...)
#             current.fields <- unique(names(unlist(test)))
#             ind <- 0
#             while (ind < length(dots)){
#               temp <- tolower(dots[[ind]])
#               if (is.numeric(temp)){
#                 x <- x[temp]
#               }else if (pmatch(temp, current.fields)){
#                 if(ind==length(dots)){ 
#                   x <- eval(parse(text=paste0('x$', temp)))
#                   ind <- ind + 1
#                 }else{
#                   pattern <- tolower(dots[[ind+1]])
#                   
#                   if (temp=='author' || temp=='editor'){  # need special handling for a/e and y/d
#                     x <- MatchAuthor(x, field, pattern, author.match)
#                   }else if (temp=='author' || temp=='date'){
#                     x <- MatchDate(x, temp, pattern, date.match)
#                   }else{
#                     x <- search(x, temp, pattern, exact = FALSE)
#                   }
#                                                        
#                   ind <- ind + 2
#                 }    
#               }else{
#                 stop('Invalid index specified')
#               }
#             }
#             x
#           })

# setMethod("table",
#           signature(x="BibEntry"),
#           function (x, field)){
#             table(unlist(temp['field']))
#           }
# }


#test <- ReadBib('~/biblatex/code/biblatexTestBib.bib', encoding='UTF-8')
# test <- ReadZotero(user='1648676', .params=list(key='7lhgvcwVq60CDi7E68FyE3br', tag='Statistics - Machine Learning'))


.BibEntryCheckBibEntry1 <- function (x, force = FALSE, check = .BibOptions$check.entries) {
  if (identical(check, FALSE))
    return(NULL)
  fields <- names(x)
  if (!force && (!.is_not_nonempty_text(x$crossref) || !.is_not_nonempty_text(x$xdata))) 
    return(NULL)
  bibtype <- attr(x, "bibtype")
  rfields <- strsplit(BibLaTeX_entry_field_db[[bibtype]], 
                      "|", fixed = TRUE)
  if (length(rfields) > 0L) {
    ok <- sapply(rfields, function(f) any(f %in% fields))
    if (any(!ok)){ 
      if (check == 'warn'){
        warning(sprintf(ngettext(sum(!ok), "A bibentry of bibtype %s has to specify the field: %s", 
                              "A bibentry of bibtype %s has to specify the fields: %s"), 
                     sQuote(bibtype), paste(rfields[!ok], collapse = ", ")), 
             domain = NA)
        return(NULL)
      }else{
        stop(sprintf(ngettext(sum(!ok), "A bibentry of bibtype %s has to specify the field: %s", 
                              "A bibentry of bibtype %s has to specify the fields: %s"), 
                     sQuote(bibtype), paste(rfields[!ok], collapse = ", ")), 
             domain = NA) 
      }
    }
  }
}

.BibEntry_match_format_style <- function (style){
    ind <- pmatch(tolower(style), tolower(bibentry_format_styles), 
        nomatch = 0L)
    if (all(ind == 0L)) 
        stop(gettextf("%s should be one of %s", sQuote("style"), 
            paste(dQuote(bibentry_format_styles), collapse = ", ")), 
            domain = NA)
    bibentry_format_styles[ind]
}

.BibEntry_expand_crossrefs <- function (x, more = list(), to.bibtex = FALSE){
  if (!length(x))
    return(NULL)
  y <- if (length(more)) 
    c(x, more)  # do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  xrefs <- lapply(x, '[[', "xdata")
  px <- which(vapply(xrefs, length, 0L) > 0L)
  if (length(px)){
    xk <- sapply(xrefs[px], strsplit, ',')
    # xdata field can be comma-separated list of keys
    x[px] <- Map(function(entry, xdat.keys, full.bib){
      pos <- match(xdat.keys, .BibEntry_get_key(full.bib)) 
      ok <- !is.na(pos)
      if (any(ok)){
        for (i in pos[ok]){
          xdat <- full.bib[[i]]
          add <- setdiff(names(xdat), names(entry))
            
          entry[add] <- xdat[add]
          if (any(add %in% .BibEntryDateField))
            attr(entry, 'dateobj') <- ProcessDates(entry)
        }
      }
      entry
    }, x[px], xk, MoreArgs = list(full.bib = y))
  }
  
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .BibEntry_get_key(y))
    ok <- !is.na(pk)
    
    if (to.bibtex){
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
    }else{
      x[pc[ok]] <- Map(ResolveBibLaTeXCrossRef, x[pc[ok]], y[pk[ok]])
    }
    
    status <- lapply(x[pc], function(e) tryCatch(.BibEntryCheckBibEntry1(e, 
                                                                           TRUE), error = identity))
    bad <- which(sapply(status, inherits, "error"))
    if (length(bad)) {
      for (b in bad) {
        warning(gettextf("Dropping invalid entry %d:\n%s", 
                         pc[b], conditionMessage(status[[b]])))
      }
      x[pc[bad]] <- NULL
    }
  }
  class(x) <- c("BibEntry", "bibentry")
  #types <- unlist(x$bibtype)
  #x[!types %in% c('Set', 'XData')]
  x
}

ResolveBibLaTeXCrossRef <- function(chi, par){
  add <- setdiff(names(par), names(chi))

  # titleaddon and subtitle in parent have special fields for child
  # ensure child with no subtitle, titleaddon don't inherit them incorrectly
  chi.type <- tolower(attr(chi, 'bibtype'))
  par.type <- tolower(attr(par, 'bibtype'))
  if (!is.na(match(chi.type, c('incollection', 'suppcollection', 'collection', 'reference', 'inreference',
                               'inbook', 'suppbook', 'bookinbook', 'book', 'inproceedings', 'proceedings',
                               'article', 'suppperiodical'))))
    add <- add[!add %in% c('subtitle', 'titleaddon')]
  chi[add] <- par[add]      
  if (any(add %in% .BibEntryDateField))
    attr(chi, 'dateobj') <- ProcessDates(chi)
  # special handling for bookauthor, maintitle, mainsubtitle, maintitleaddon, booktitle, booktitleaddon, 
  #  booksubtitle, journaltitle, journalsubtitle; see Appendix B of biblatex manual
  if (!is.na(match(par.type, c('mvbook', 'book'))) && 
        !is.na(match(chi.type, c('inbook', 'bookinbook', 'suppbook'))) && is.null(chi$bookauthor)) 
    chi$bookauthor <- par$author
  
  if (par.type == 'mvbook' && !is.na(match(chi.type, c('book', 'inbook', 'bookinbook', 'suppbook')))){
    if (is.null(chi$maintitle))
      chi$maintitle <- par$title
    if (is.null(chi$mainsubtitle))
      chi$mainsubtitle <- par$subtitle
    if (is.null(chi$maintitleaddon))
      chi$maintitleaddon <- par$titleaddon
  }else if (par.type == 'mvcollection' && !is.na(match(chi.type, c('collection', 'reference', 'incollection')))){
    if (is.null(chi$maintitle))
      chi$maintitle <- par$title
  }else if (par.type == 'mvreference' && !is.na(match(chi.type, c('inreference', 'suppcollection')))){
    if (is.null(chi$mainsubtitle))
      chi$mainsubtitle <- par$subtitle
    if (is.null(chi$maintitleaddon))
      chi$maintitleaddon <- par$titleaddon
  }else if (par.type == 'mvproceedings' && !is.na(match(chi.type, c('proceedings', 'inproceedings')))){
    if (is.null(chi$maintitle))
      chi$maintitle <- par$title
    if (is.null(chi$mainsubtitle))
      chi$mainsubtitle <- par$subtitle
    if (is.null(chi$maintitleaddon))
      chi$maintitleaddon <- par$titleaddon
  }else if (par.type == 'book' && !is.na(match(chi.type, c('inbook', 'bookinbook', 'suppbook')))){
    if (is.null(chi$booktitle))
      chi$booktitle <- par$title
    if (is.null(chi$booksubtitle))
      chi$booksubtitle <- par$subtitle
    if (is.null(chi$booktitleaddon))
      chi$booktitleaddon <- par$titleaddon
  }else if (par.type == 'collection' && !is.na(match(chi.type, c('incollection', 'inreference')))){
    if (is.null(chi$booktitle))
      chi$booktitle <- par$title
  }else if (par.type == 'reference' && chi.type == 'suppcollection'){
    if (is.null(chi$booksubtitle))
      chi$booksubtitle <- par$subtitle
    if (is.null(chi$booktitleaddon))
      chi$booktitleaddon <- par$titleaddon
  }else if (par.type == 'proceedings' && chi.type == 'inproceedings'){
    if (is.null(chi$booktitle))
      chi$booktitle <- par$title
    if (is.null(chi$booksubtitle))
      chi$booksubtitle <- par$subtitle
    if (is.null(chi$booktitleaddon))
      chi$booktitleaddon <- par$titleaddon
  }else if (par.type == 'periodical' && !is.na(match(chi.type, c('article', 'suppperiodical')))){
    if (is.null(chi$journaltitle) && is.null(journal))
      chi$journaltitle <- par$title
    if (is.null(chi$journalsubtitle))
      chi$journalsubtitle <- par$subtitle
  }
  chi
}

ArrangeAuthors <- function (x){
  rx <- "[[:space:]]+and[[:space:]]+"
  x <- gsub('[[:space:]]{2,}', ' ', x)
  authors <- lapply(strsplit(x, rx)[[1]], ArrangeSingleAuthor)
  as.personList(authors)
}

# ArrangeSingleAuthor <- function(y){
#   if( grepl( ",", y) ) {
#     y <- sub( "^([^,]+)[[:space:]]*,[[:space:]]*(.*?)$", "\\2 \\1", y , perl = TRUE )
#   }
#   rx <-  "^[{](.*)[}]$"
#   rx2 <- "^([^]]*)[{]([^]]*)[}]$"
#   if( grepl( rx, y ) ) {
#     person( sub( rx, "\\1", y ) )
#   } else if( grepl( rx2, y ) ) {
#     person( 
#       sub( rx2, "\\1", y ), 
#       sub( rx2, "\\2", y )
#     )
#   } else {
#     as.person( y )
#   }
# } 

ArrangeSingleAuthor <- function(y){
  #browser()
  if (grepl('[\\]', y)){
    tmp <- try(parseLatex(y), TRUE)
    if (!inherits(tmp, 'try-error'))
      y <- deparseLatex(latexToUtf8(tmp))
  }
  #   if( grepl( ",", y) ) {
  #     y <- sub( "^([^,]+)[[:space:]]*,[[:space:]]*(.*?)$", "\\2 \\1", y , perl = TRUE )
  #   }
  parts <- unlist(strsplit(y, ','))
  len.parts <- length(parts)
  if (len.parts == 1L){
    #     parts <- "{Barnes} {and} {Noble,} {Inc.}"
    
    if (grepl('[}]$', parts)){
      s <- unlist(strsplit(parts, ''))
      i <- length(s) - 1L
      paren <- 1
      while (paren > 0 && i > 0){
        if (s[i] == '{'){
          paren <- paren - 1L
        }else if (s[i] == '}'){
          paren <- paren + 1L
        }
        i <- i - 1L
      }
      last <- paste0(s[(i+2):(length(s)-1)], collapse = '')
      first <- NULL
      if (i > 0)
        first <- paste0(s[1:(i-1)], collapse = '')
      person(UnlistSplitClean(first), cleanupLatex(last))  # Mathew {McLean IX}
    }else{
      vonrx <- "(^|[[:space:]])([[:lower:]+[:space:]?]+)[[:space:]]"
      m <- regexec(vonrx, parts)
      von <- unlist(regmatches(parts, m))[3L]
      if (!is.na(von)){
        name <- unlist(strsplit(parts, vonrx))
        if (length(name) == 1L){  # von Bommel
          person(family=c(cleanupLatex(von), cleanupLatex(name)))
        }else{  # Mark von Bommel
          person(given = UnlistSplitClean(name[1L]), family=c(cleanupLatex(von), cleanupLatex(name[2L])))
        }
      }else{  # George Bernard Shaw
        name <- UnlistSplitClean(parts)
        len.name <- length(name)
        if (len.name == 1){
          person(family = name)
        }else{
          person(given = name[1L:(len.name - 1L)], family = name[len.name])
        }
      }
    }
  }else if (len.parts == 2L){
    if (grepl('^[{]', parts[1L])){  # e.g. {de Gama}, Vasco
      person(UnlistSplitClean(parts[2L]), UnlistSplitClean(parts[1L]))
    }else{
      vonrx <- "^([[:lower:]+[:space:]?]+)[[:space:]]"
      m <- regexec(vonrx, parts[1L])
      von <- unlist(regmatches(parts[1L], m))[2]
      if (is.na(von)){  # e.g. Smith, John Paul
        person(UnlistSplitClean(parts[2L]), cleanupLatex(parts[1L]))
      }else{  # e.g. de la Soul, John
        person(UnlistSplitClean(parts[2L]), c(cleanupLatex(von), cleanupLatex(sub(vonrx, '', parts[1L]))))
      }
    }
  }else if (len.parts == 3L){
    vonrx <- "^([[:lower:]+[:space:]?]+)[[:space:]]"
    m <- regexec(vonrx, parts[1L])
    von <- unlist(regmatches(parts[1L], m))[2]
    if (is.na(von)){  # e.g. White, Jr., Walter
      person(UnlistSplitClean(parts[3L]), c(cleanupLatex(parts[1L]), cleanupLatex(parts[2L])))
    }else{  # e.g. des White, Jr., Walter
      person(UnlistSplitClean(parts[3L]), 
             c(cleanupLatex(von), cleanupLatex(sub(vonrx, '', parts[1L])), cleanupLatex(parts[2L])))
    }
  }else{
    stop('Invalid name format in bibentry.')
  }
} 

UnlistSplitClean <- function(s){
  #cleanupLatex(str_trim(s))
  unlist(strsplit(gsub("[{}]", "", str_trim(s)), " "))
}

# cleanupLatex <- function (x){
#   if (!length(x)) 
#     return(x)
#   x <- gsub('mkbibquote', 'dQuote', x)
#   x <- gsub('\\\\hyphen', '-', x)
#   latex <- try(tools::parseLatex(x), silent = TRUE)
#   if (inherits(latex, "try-error")) {
#     x
#   }else {
#     tools::deparseLatex(tools::latexToUtf8(latex), dropBraces = TRUE)
#   }
# }

cleanupLatex <- function (x){
  if (!length(x)) 
    return(x)
  
  if (any(grepl('mkbib', x))){
    x <- gsub('mkbibquote', 'dQuote', x)
    x <- gsub('mkbibemph', 'emph', x)
    x <- gsub('mkbibbold', 'bold', x)
  }
  x <- gsub('\\\\hyphen', '-', x)
  
  latex <- try(tools::parseLatex(x), silent = TRUE)
  if (inherits(latex, "try-error")) {
    x
  }else {
    x <- tools::deparseLatex(tools::latexToUtf8(latex), dropBraces = TRUE)
    if (grepl("\\\\[[:punct:]]", x)){
      x <- gsub("\\\\'I", '\u00cd', x)
      x <- gsub("\\\\'i", '\u00ed', x)
      x <- gsub('\\\\"I', '\u00cf', x)
      x <- gsub('\\\\"i', '\u00ef', x)
      x <- gsub("\\\\\\^I", '\u00ce', x)
      x <- gsub("\\\\\\^i", '\u00ee', x)
      x <- gsub("\\\\`I", '\u00cc', x)
      x <- gsub("\\\\`i", '\u00ec', x)
      Encoding(x) <- 'UTF-8'
    }
    x
  }
}

MakeCitationList <- function( x, header, footer){
    rval <- list()
    for (i in seq_along(x)){
      if (!is.null(x[[i]]))
        rval <- c(rval, unclass(x[[i]]))
    }
    class(rval) <- c("BibEntry", "bibentry" )
    rval
}

.is_not_nonempty_text <- function(x){
  is.null(x) || any(is.na(x)) || all(grepl("^[[:space:]]*$", x))
}

.listify <- function (x) {
  if (inherits(x, "list")) x else list(x)
}

bibentry_attribute_names <- c("bibtype", "textVersion", "header", "footer", "key", "dateobj")
bibentry_format_styles <- c("text", "Bibtex", "citation", "html", "latex", "textVersion", "R")

# from utils:::toBibtex, good for matching by given name initials only
format_author <- function(author) paste(sapply(author, function(p) {
  fnms <- p$family
  only_given_or_family <- is.null(fnms) || is.null(p$given)
  fbrc <- if (length(fnms) > 1L || any(grepl("[[:space:]]", 
                                             fnms)) || only_given_or_family) 
    c("{", "}")
  else ""
  gbrc <- if (only_given_or_family) 
    c("{", "}")
  else ""
  format(p, include = c("given", "family"), braces = list(given = gbrc, 
                                                          family = fbrc))
}), collapse = " and ")

bibentry_list_attribute_names <- c("mheader", "mfooter", "strings")

.BibEntry_get_key <- function (x) {
  if (!length(x)) 
    return(character())
  keys <- lapply(unclass(x), attr, "key")
  keys[!vapply(keys, length, 0L)] <- ""
  unlist(keys)
}

ParseGSCites <- function(l, encoding, check.entries=.BibOptions$check.entries) {
  td <- l[[1L]]
  title <- xmlValue(td[[1L]], encoding)
  author <- xmlValue(td[[3L]], encoding)
  cited_by <- as.numeric(xmlValue(l[[2L]][[1L]], encoding))
  if (is.na(cited_by))  # no citation yet
    cited_by <- "0"
  year <- as.numeric(xmlValue(l[[4L]], encoding))
  src <- xmlValue(td[[5L]])
  first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", 
                                    src)) - 1L
  ids <- which(first_digit < 0L)
  first_digit <- replace(first_digit, ids, str_length(src)[ids])
  journal <- str_trim(str_sub(src, 1L, first_digit))
  trailing_commas <- as.numeric(regexpr(",$", journal)) - 1L
  ids <- which(trailing_commas < 0L)
  trailing_commas <- replace(trailing_commas, ids, 
                             str_length(journal)[ids])
  journal <- str_sub(journal, 1L, trailing_commas)
  numbers <- str_trim(str_sub(src, first_digit + 1L, 
                              str_length(src)))
  
  # handle '...' in title, journal, or authors
  if (!identical(check.entries, FALSE)){
    if (is.null(title <- CheckGSDots(title, title, check.entries)) || 
          is.null(author <- CheckGSDots(author, title, check.entries)) ||
          is.null(journal <- CheckGSDots(journal, title, check.entries)))
      return(NA)
  }
  
  res <- list(title = title, author = author, 
              journal = journal, number = numbers, cites = cited_by, 
              year = year)
  if(res$number==''){  # assume book entry if no number
    attr(res, 'entry') <- 'book'
    res$number <- NULL
    res$publisher <- res$journal
    res$journal <- NULL
  }else{
    attr(res, 'entry') <- 'article'
    numbers <- ProcessGSNumbers(res$number)
    res$number <- numbers$number
    res$pages <- numbers$pages
    res$volume <- numbers$volume
  }
  
  # create key
  aut <- tolower(strsplit(res$author, ' ')[[1]][2])  # get last name of first author
  aut <- gsub(',', '', aut)  # remove trailing commas
  first.word <- tolower(strsplit(res$title, ' ')[[1]][[1]])  # get first word of title
  attr(res, 'key') <- paste0(aut, res$year, first.word)
  
  res$author <- ProcessGSAuthors(res$author)  # format authors for MakeBibEntry
  
  return(res)
}

ProcessGSAuthors <- function(authors){
  authors <- gsub(',', ', and', authors)  # add "and" to separate authors
  authors <- gsub('([A-Z])([A-Z])', '\\1 \\2', authors)  # add space between given name initials
  
  return(as.personList(authors))
}

ProcessGSNumbers <- function(numbers){
  pages <- volume <- number <- NULL
  
  m <- regexpr('([0-9]+)', numbers)
  if(m != -1)
    volume <- regmatches(numbers, m)

  m <- regexpr('[(]([0-9]+)[)]', numbers)
  if(m != -1){
    number <- regmatches(numbers, m)
    number <- substr(number, 2, nchar(number)-1)  # remove ( )
  }
  
  m <- regexpr('[0-9]+[\\-][0-9]+', numbers)
  if(m != -1){
    pages <- regmatches(numbers, m)
    pages <- gsub('-', '--', pages)  # '-' --> '--'
  }
  
  return(list(pages = pages, number = number, volume = volume))
}

CheckGSDots <- function(x, title, check){
  tx <- gsub(' [.]{3,}$', '', x)
  if(tx != x){
    entry <- deparse(substitute(x))
    if (check == 'warn'){
      message(paste0('Incomplete ', entry, ' information for entry \"', title, '\" adding anyway'))
      return(tx)
    }else{
      message(paste0('Incomplete ', entry, ' information for entry \"', title, '\" it will NOT be added'))
      return()
    }
  }else{
    return(tx)
  }
}

MakeBibEntry <- function (x, to.person = TRUE) {
  type <- attr(x, "entry")
  key <- attr(x, "key")
  y <- as.list(x)
  names(y) <- tolower(names(y))
  
  if (to.person){
    lapply(.BibEntryNameList, function(fld){
                 if (fld %in% names(y)) 
                    y[[fld]] <<- ArrangeAuthors(y[[fld]])
             })
  }else{
    lapply(.BibEntryNameList, function(fld){
             if (fld %in% names(y)) 
                y[[fld]] <<- as.person(y[[fld]])
         })
  }
  
  tdate <- NULL
  if (type != 'set')
    tdate <- ProcessDates(y)

  res <- try(BibEntry(bibtype = type, key = key, dateobj = tdate, other = y), TRUE)
  if (inherits(res, 'try-error')){
    if(!is.null(y[['title']])){
      message(paste0('Ignoring entry titled \"', y[['title']], '\" because ', strsplit(res, '\\n[[:space:]]*')[[1]][2]))
      #message(strsplit(res, '\n'), '\n')  # relies on bibentry errors being two lines      
    }else{
      message(paste0('Ignoring entry with key \"', key, '\" because ', strsplit(res, '\\n[[:space:]]*')[[1]][2]))
      # message(strsplit(res, '\n'), '\n')  # relies on bibentry errors being two lines      
    }
    return(NULL)
  }
#   if (!(is.null(tdate) || inherits(tdate, 'try-error') || is.na(tdate)))
#     attr(res, 'dateobj') <- tdate

  return(res)
}

ProcessDates <- function(bib){
  tdate <- try(ProcessDate(bib[['date']], NULL), TRUE)
   
  if (inherits(tdate, 'try-error') || is.null(tdate)){
    tdate <- try(ProcessDate(bib[['year']], bib[['month']]), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- try(ProcessDate(bib[['eventdate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))  
      tdate <- try(ProcessDate(bib[['origdate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))  
      tdate <- try(ProcessDate(bib[['urldate']], NULL), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate))
      tdate <- NULL
  }

  return(tdate)
}

#' @importFrom lubridate new_interval
ProcessDate <- function(dat, mon, searching = FALSE){
  if (!length(dat))
    return()
  
  .day <- FALSE
  .mon <- FALSE
  if (length(grep('^(1|2)[0-9]{3}$', dat))){
    if (!is.null(mon)){
      res <- as.POSIXct(paste0(dat, '-', mon, '-01'))
      .mon <- TRUE
    }else{
      res <- as.POSIXct(paste0(dat, '-01-01'))
    }
  }else if (length(grep('^(1|2)[0-9]{3}/$', dat))){
    if (!is.null(mon)){
      res <- new_interval(paste0(substring(dat, 1, 4), '-', mon, '-01'), Sys.Date())
      .mon <- TRUE
    }else{
      res <- new_interval(paste0(substring(dat, 1, 4), '-01-01'), Sys.Date())
    }
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]/$', dat))){
    res <- new_interval(paste0(substring(dat, 1, 7), '-01'), Sys.Date())
    .mon <- TRUE
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]/$', dat))){
    res <- new_interval(substring(dat, 1, 10), Sys.Date())
    .mon <- .day <- TRUE
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]$', dat))){
    res <- as.POSIXct(paste0(dat, '-01'))
    .mon <- TRUE
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]$', dat))){
    res <- as.POSIXct(dat)
    .day <- .mon <- TRUE
  }else if (length(grep('^(1|2)[0-9]{3}/(1|2)[0-9]{3}$', dat))){
    res <- new_interval(paste0(substring(dat, 1, 4), '-01-01'), paste0(substring(dat, 6, 9), '-01-01'))
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]/(1|2)[0-9]{3}-[01][0-9]$', dat))){
    res <- new_interval(paste0(substring(dat, 1, 7), '-01'), paste0(substring(dat, 9, 15), '-01'))
    .mon <- TRUE
  }else if (length(grep('^(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]/(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]$', dat))){
    res <- new_interval(substring(dat, 1, 10), substring(dat, 12, 21))
    .day <- .mon <- TRUE
  }else if (searching){
    if (length(grep('^/(1|2)[0-9]{3}$', dat))){
      res <- new_interval('0001-01-01', paste0(substring(dat, 2, 5), '-01-01'))
    }else if (length(grep('^/(1|2)[0-9]{3}-[01][0-9]$', dat))){
      res <- new_interval('0001-01-01', paste0(substring(dat, 2, 8), '-01'))
    }else if (length(grep('^/(1|2)[0-9]{3}-[01][0-9]-[0-3][0-9]$', dat))){
      res <- new_interval('0001-01-01', substring(dat, 2, 11))
    }else{
      stop('No valid date format available.')
    }
  }else{
    stop('No valid date format available.')
  }
  attr(res, 'day.mon') <- .day + .mon
  return(res)
}

CreateBibKey <- function(ti, au, yr){
  m <- regexpr('\\<([[:alpha:]]{4,})\\>', ti)
  key.title <- tolower(regmatches(ti, m))  # will be character(0) if no matches or if ti is NULL
  if (inherits(au, 'person'))
    au <- gsub(' ', '', tolower(au[1]$family[1]))

  res <- paste0(au, yr, key.title)
  if (!length(res))
    return()
  
  return(res)
}

# Clean up LaTeX accents and braces
# cleanupLatex <- function(x) {
#     if (!length(x)) return(x)
#     latex <- tryCatch(parseLatex(x), error = function(e)e)
#     if (inherits(latex, "error")) {
#       x
#     } else {
#     	deparseLatex(latexToUtf8(latex), dropBraces=TRUE)
#     }
# }

.BibEntryNameList <- c('author', 'editor', 'editora', 'editorb', 'editorc', 'translator', 'commentator', 'annotator',
             'introduction', 'foreword', 'afterword', 'bookauthor', 'holder')
.BibEntryDateField <- c('date', 'year', 'month', 'eventdate', 'origdate', 'urldate')
.BibEntryTypeField <- c(mathesis = 'MA Thesis', phdthesis = 'PhD thesis', datacd = 'CD-ROM',
                        candthesis = 'Cand. thesis', techreport = 'Tech. rep.', 
                        resreport = 'Research rep.', software = 'Comp. software', audiocd = 'Audio CD')
