library(lubridate)

CompareDates <- function(bib.entry, date2, compare.op='==', match.date = 'yearonly'){
  date1 <- bib.entry$date
  if(is.null(date1))
    return(FALSE)
  #browser()
  if(match.date != 'exact'){
    date1 <- year(date1)
  }
  
 
  return(do.call(compare.op, list(date1, date2)))
}

MatchDate <- function(x, pattern){
 # browser()
  match.date <- 'yearonly'  # .BibOptions()$
  x <- unclass(x)
  if(length(grep('--', pattern))){  # range of dates specified, perform search twice
    pos <- regexpr('--', pattern)[1]
    str1 <- substr(pattern, 1, pos-1)
    d2 <- as.Date(switch(as.character(nchar(str1)),
          '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
          '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
                str1))
    if(match.date != 'exact')
      d2 <- year(d2)       
    
    match.pos <- sapply(x, CompareDates, date2=d2, compare.op ='>=')
    if(sum(match.pos)==0){ # MatchDate returned false for all bib entries
      print('No matches')
      return()
    }  
    
    # if matches for >= low year, before search for <= high year
    x <- x[match.pos]
    str1 <- substr(pattern, pos+2, nchar(pattern))
    comp.op <- '<='
  }else if ((comp.op <- substr(pattern, 1, 2))=='<=' || comp.op=='>='){
    str1 <- substr(pattern, 3, nchar(pattern))
  }else if ((comp.op <- substr(pattern, 1, 1)) == '>' || comp.op == '<'){
    str1 <- substr(pattern, 2, nchar(pattern))
  }else{  # exact date search requested
    comp.op <- '=='
    str1 <- pattern
  }
  
  d2 <- as.Date(switch(as.character(nchar(str1)),
          '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
          '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
                str1))
  if(match.date != 'exact')
    d2 <- year(d2)  
  
  match.pos <- sapply(x, CompareDates, date2=d2, compare.op =comp.op)
  if(sum(match.pos)==0){
    print('No Matches')
    return()
  }
  
  return(match.pos)
}

# UTF-8 compatible
MatchAuthor <- function(x, field, pattern, match.author='exact'){
  return('hello')
}

CompareEntries <- function(entry, searchterms, exact = FALSE, unicode=TRUE){
  if(is.null(entry))
    return(FALSE)
  
  if(!exact){
    entry <- tolower(entry)
    searchterms <- tolower(searchterms)
  }
  #fun <- ifelse(exact, 'match', 'pmatch')
  
  return(all(match(x=searchterms, table=entry, nomatch=FALSE)))
}

# make UTF-8 compatible
# does not work: setGeneric('search', signature=)
SearchField <- function(x, field, pattern){
  # this code loses correct position of matches
  # match(unlist(eval(parse(text=paste0('x$', field)))), pattern, nomatch=FALSE)
  
  match.pos <- sapply(eval(parse(text=paste0('x$', field))), CompareEntries, searchterms=pattern, 
                      exact = FALSE, unicode=TRUE)
  
  return(match.pos)
}

SearchBib <- function(x, ..., match.date = 'yearonly', match.author='lastonly'){
  
  # opt.def <- BibOptions()
  # BibOptions()$match.date <- match.date
  # BibOptions()$match.author <- match.author
  fcall <- match.call()
  fcall$return.ind <- TRUE
  fcall[[1L]] <- as.name('[.BibEntry')
  # BibOptions() <- opt.def
  return(eval(fcall))
}

# TO Do: x['keyval']
`[.BibEntry` <- function(x, ..., return.ind=FALSE, drop=TRUE){
 # browser()
  if(!length(x))
    return(x)
  # browser()
  dots <- list(...)
  current.fields <- c(unique(names(unlist(x))), 'bibtype', 'key')
  
  if(length(dots)==1){
    dot.arg <- dots[[1]]
   # browser()
    if(is.numeric(dot.arg) || is.logical(dot.arg)){  # simple subsetting
      return(x[[dot.arg]])
    }else if (is.list(dot.arg)){  # call "[" again with unlisted args
      args <- as.list(sapply(1:length(dot.arg), function(i) c(names(dot.arg)[i], dot.arg[[i]])))
      args$x <- x
      return(do.call('[.BibEntry', args))
    }else if (is.character(dot.arg)){  
      dot.arg <- tolower(dot.arg)
     # browser()
      if (dot.arg %in% current.fields){
        res <- eval(parse(text=paste0('x$', dot.arg)))
        names(res) <- names(x)
        return(unlist(res))
      }else{  # assumed to be keys
        return(x[[names(x) %in% dot.arg]])
      }
    }else{
      stop('Invalid argument')
    }
  }
  # dots$x <- NULL
  # browser()
  temp <- dots[[1]]
#   if (is.numeric(temp)){ # simple subset use bibentry's "["
#     print('WOW YOU FOUND ME!!!')
#     class(x) <- 'bibentry'
#     dots <- dots[-1]
#     dots$x <- x[temp]
#     class(dots$x) <- c('BibEntry', 'bibentry')
#   }else 
  if (pmatch(temp <- tolower(temp), current.fields, nomatch=0)){
#     if(length(dots)==1){ 
#       print('WOW YOU FOUND THE OTHER ME!!!')
#       dots$x <- eval(parse(text=paste0('x$', temp)))
#     }else{
      pattern <- dots[[2]]
      
      if (temp=='author' || temp=='editor'){  # need special handling for a/e and y/d
        match.pos <- MatchAuthor(x, field = temp, pattern = pattern)
      }else if (temp == 'year' || temp == 'date'){
        match.pos <- MatchDate(x, pattern = pattern)
      #  }else if (temp == 'bibtype'){
      #  if(length(pattern) > 1)
      #    stop('Search term for bibtype must be length one since each entry can have only one type')
      #  dots$x <- x[as.logical(mapply(function(x, table) match(tolower(attr(unclass(x)[[1]], 'bibtype')), 
      #                                            pattern, nomatch=FALSE ), x=x, table=pattern))]
      }else{
        match.pos <- SearchField(x, field=temp, pattern=pattern)
      }
      dots <- dots[-c(1, 2)]
#    }    
  }else{
    stop('Invalid argument')
  }
  
  if(sum(match.pos)==0){
    message('No matches')
    return()
  }
  
  if (length(dots) > 1){ # perform recursion
    dots$x <- x[match.pos]
    dots$x <- do.call("[.BibEntry", dots) 
  }
  if(return.ind){
    return(which(match.pos))
  }else{
    x <- x[match.pos]
    class(x) <- c('BibEntry', 'bibentry')
    return(x)  
  }
  
}
