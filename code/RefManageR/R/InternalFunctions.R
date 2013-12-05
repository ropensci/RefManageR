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


.BibEntryCheckBibEntry1 <- function (x, force = FALSE) {
  fields <- names(x)
  if (!force && !.is_not_nonempty_text(x$crossref)) 
    return(NULL)
  bibtype <- attr(x, "bibtype")
  rfields <- strsplit(BibLaTeX_entry_field_db[[bibtype]], 
                      "|", fixed = TRUE)
  if (length(rfields) > 0L) {
    ok <- sapply(rfields, function(f) any(f %in% fields))
    if (any(!ok)) 
      stop(sprintf(ngettext(sum(!ok), "A bibentry of bibtype %s has to specify the field: %s", 
                            "A bibentry of bibtype %s has to specify the fields: %s"), 
                   sQuote(bibtype), paste(rfields[!ok], collapse = ", ")), 
           domain = NA)
  }
}

.BibEntry_expand_crossrefs <- function (x, more = list()) {
  y <- if (length(more)) 
    do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .bibentry_get_key(y))
    ok <- !is.na(pk)
    x[pc[ok]] <- Map(function(u, v) {
      add <- setdiff(names(v), names(u))
      u[add] <- v[add]
      if (!is.na(match(tolower(attr(u, "bibtype")), c("incollection", 
                                                      "inproceedings"))) && is.null(u$booktitle)) 
        u$booktitle <- v$title
      u
    }, x[pc[ok]], y[pk[ok]])
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
  class(x) <- C("BibEntry", "bibentry")
  x
}

.bibentry_expand_crossrefs <- function (x, more = list()) {
  y <- if (length(more)) 
    do.call(c, c(list(x), more))
  else x
  x <- unclass(x)
  y <- unclass(y)
  crossrefs <- lapply(x, `[[`, "crossref")
  pc <- which(vapply(crossrefs, length, 0L) > 0L)
  if (length(pc)) {
    pk <- match(unlist(crossrefs[pc]), .bibentry_get_key(y))
    ok <- !is.na(pk)
    x[pc[ok]] <- Map(function(u, v) {
      add <- setdiff(names(v), names(u))
      u[add] <- v[add]
      if (!is.na(match(tolower(attr(u, "bibtype")), c("incollection", 
                                                      "inproceedings"))) && is.null(u$booktitle)) 
        u$booktitle <- v$title
      u
    }, x[pc[ok]], y[pk[ok]])
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
  class(x) <- "bibentry"
  x
}

ArrangeAuthors <- function (x) {
  rx <- "[[:space:]]+and[[:space:]]+"
  authors <- lapply(strsplit(x, rx)[[1]], ArrangeSingleAuthor)
  as.personList(authors)
}

ArrangeSingleAuthor <- function(y)
  {
    if( grepl( ",", y) ) {
      y <- sub( "^([^,]+)[[:space:]]*,[[:space:]]*(.*?)$", "\\2 \\1", y , perl = TRUE )
    }
    rx <-  "^[{](.*)[}]$"
    rx2 <- "^([^]]*)[{]([^]]*)[}]$"
    if( grepl( rx, y ) ) {
      person( sub( rx, "\\1", y ) )
    } else if( grepl( rx2, y ) ) {
      person( 
        sub( rx2, "\\1", y ), 
        sub( rx2, "\\2", y )
      )
    } else {
      as.person( y )
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

sort.BibEntry <- function (x, decreasing = FALSE, .bibstyle = NULL, drop = FALSE, 
          ...) 
{
  x[order(tools::bibstyle(.bibstyle)$sortKeys(x), decreasing = decreasing), 
    drop = drop]
}

bibentry_attribute_names <- c("bibtype", "textVersion", "header", "footer", "key")

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

bibentry_list_attribute_names <- c("mheader", "mfooter")

.bibentry_get_key <- function (x) 
{
  if (!length(x)) 
    return(character())
  keys <- lapply(unclass(x), attr, "key")
  keys[!vapply(keys, length, 0L)] <- ""
  unlist(keys)
}

ParseGSCites <- function(l, encoding, bib.violation=.BibOptions$bib.violation) {
  td <- l[[1]]
  title <- xmlValue(td[[1]], encoding)
  author <- xmlValue(td[[3]], encoding)
  cited_by <- as.numeric(xmlValue(l[[2]][[1]], encoding))
  year <- as.numeric(xmlValue(l[[4]], encoding))
  src <- xmlValue(td[[5]])
  first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", 
                                    src)) - 1
  ids <- which(first_digit < 0)
  first_digit <- replace(first_digit, ids, str_length(src)[ids])
  journal <- str_trim(str_sub(src, 1, first_digit))
  trailing_commas <- as.numeric(regexpr(",$", journal)) - 1
  ids <- which(trailing_commas < 0)
  trailing_commas <- replace(trailing_commas, ids, 
                             str_length(journal)[ids])
  journal <- str_sub(journal, 1, trailing_commas)
  numbers <- str_trim(str_sub(src, first_digit + 1, 
                              str_length(src)))
  
  # handle '...' in title, journal, or authors
  if (is.null(title <- CheckGSDots(title, title)) || 
          is.null(author <- CheckGSDots(author, title)) ||
          is.null(journal <- CheckGSDots(journal, title)))
    return(NA)
  
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

CheckGSDots <- function(x, title){
  tx <- gsub(' [.]{3,}$', '', x)
  if(tx != x){
    entry <- deparse(substitute(x))
    if(.BibOptions$bib.violation != 'error'){
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
  
  if ("author" %in% names(y) && to.person) {
    y[["author"]] <- ArrangeAuthors(y[["author"]])
  }
  if ("editor" %in% names(y)  && to.person) {
    y[["editor"]] <- ArrangeAuthors(y[["editor"]])
  }
  
  if ("date" %in% names(y)){
    if (!inherits(y[['date']], "POSIXlt"))
      y[['date']] <- as.Date(switch(as.character(nchar(y[['date']])),
                                  '4' = paste0(y[['date']], '-01-01'),    # needed to get around R assigning current day and month when unspecified
                                  '7' = paste0(y[['date']], '-01'),  # %Y-%d which doesn't work with strptime
                                  y[['date']]))
  }else if ("year" %in% names(y)){
    y[["date"]] <- as.Date(paste0(y[["year"]], '-01-01'))
  }
#  browser()
  res <- try(BibEntry(bibtype = type, key = key, other = y), TRUE)
  if (inherits(res, 'try-error')){
    if(!is.null(y[['title']])){
      message(paste0('Ignoring entry ', y[['title']], ' because'))
      message(strsplit(res, '\n'), '\n')  # relies on bibentry errors being two lines      
    }else{
      message(paste0('Ignoring entry ', key, ' because'))
      message(strsplit(res, '\n'), '\n')  # relies on bibentry errors being two lines      
    }
    return(NULL)
  }
  return(res)
}