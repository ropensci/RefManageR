library(lubridate)

# CompareDates <- function(bib.entry, date2, compare.op='==', match.date = 'year.only'){
#   date1 <- bib.entry$dateobj
#   if(is.null(date1))
#     return(FALSE)
#   #browser()
#   if(match.date != 'exact'){
#     date1 <- year(date1)
#   }
#   
#  
#   return(do.call(compare.op, list(date1, date2)))
# }

# MatchDate <- function(x, pattern){
#  # browser()
#   match.date <- 'yearonly'  # .BibOptions()$
#   x <- unclass(x)
#   if(length(grep('--', pattern))){  # range of dates specified, perform search twice
#     pos <- regexpr('--', pattern)[1]
#     str1 <- substr(pattern, 1, pos-1)
#     d2 <- as.POSIXct(switch(as.character(nchar(str1)),
#           '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
#           '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
#                 str1))
#     if(match.date != 'exact')
#       d2 <- year(d2)       
#     
#     match.pos <- sapply(x, CompareDates, date2=d2, compare.op ='>=')
#     if(sum(match.pos)==0){ # MatchDate returned false for all bib entries
#       print('No matches')
#       return()
#     }  
#     
#     # if matches for >= low year, before search for <= high year
#     x <- x[match.pos]
#     str1 <- substr(pattern, pos+2, nchar(pattern))
#     comp.op <- '<='
#   }else if ((comp.op <- substr(pattern, 1, 2)) == '<=' || comp.op == '>='){
#     str1 <- substr(pattern, 3, nchar(pattern))
#   }else if ((comp.op <- substr(pattern, 1, 1)) == '>' || comp.op == '<'){
#     str1 <- substr(pattern, 2, nchar(pattern))
#   }else{  # exact date search requested
#     comp.op <- '=='
#     str1 <- pattern
#   }
#   
#   d2 <- as.POSIXct(switch(as.character(nchar(str1)),
#           '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
#           '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
#                 str1))
#   if (match.date != 'exact')
#     d2 <- year(d2)  
#   
#   match.pos <- sapply(x, CompareDates, date2=d2, compare.op =comp.op)
#   if (sum(match.pos) == 0){
#     print('No Matches')
#     return()
#   }
#   
#   return(match.pos)
# }

# CompareAuthors <- function(entry, searchterms, match.author='family.name', unicode=TRUE){
#   if(is.null(entry))
#     return(FALSE)
#   
#   if(match.author=='family.name'){
#     entry <- tolower(entry)
#     searchterms <- tolower(searchterms)
#   }
#   #fun <- ifelse(exact, 'match', 'pmatch')
#   return(all())
#   return(all(match(x=searchterms, table=entry, nomatch=FALSE)))
# }

MatchDate <- function(x, pattern, match.date = .BibOptions$match.date){
  # browser()
  if (is.null(x))
    return(FALSE)
  
  if (match.date == 'year.only'){
    if (is.interval(x)  && is.interval(pattern)){
      return(year(int_start(x)) >= year(int_start(pattern)) && year(int_end(x)) <= year(int_end(pattern)))
    }else if (is.interval(x)){
      return(year(int_start(x)) >= year(pattern) && year(int_end(x)) <= year(pattern))
    }else if (is.interval(pattern)){
      return(year(x) >= year(int_start(pattern)) && year(x) <= year(int_end(pattern)))
    }else{
      return(year(x) == year(pattern))
    }
  }else{
    if (is.interval(pattern)){
      return(x %within% pattern)
    }else{
      if (is.interval(x)){
        return(pattern %within% x)
      }else{
        return(x == pattern)  
      }
    }
  }
}
#   
#   x <- unclass(x)
#   if(length(grep('--', pattern))){  # range of dates specified, perform search twice
#     pos <- regexpr('--', pattern)[1]
#     str1 <- substr(pattern, 1, pos-1)
#     d2 <- as.POSIXct(switch(as.character(nchar(str1)),
#                             '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
#                             '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
#                             str1))
#     if(match.date != 'exact')
#       d2 <- year(d2)       
#     
#     match.pos <- sapply(x, CompareDates, date2=d2, compare.op ='>=')
#     if(sum(match.pos)==0){ # MatchDate returned false for all bib entries
#       print('No matches')
#       return()
#     }  
#     
#     # if matches for >= low year, before search for <= high year
#     x <- x[match.pos]
#     str1 <- substr(pattern, pos+2, nchar(pattern))
#     comp.op <- '<='
#   }else if ((comp.op <- substr(pattern, 1, 2)) == '<=' || comp.op == '>='){
#     str1 <- substr(pattern, 3, nchar(pattern))
#   }else if ((comp.op <- substr(pattern, 1, 1)) == '>' || comp.op == '<'){
#     str1 <- substr(pattern, 2, nchar(pattern))
#   }else{  # exact date search requested
#     comp.op <- '=='
#     str1 <- pattern
#   }
#   
#   d2 <- as.POSIXct(switch(as.character(nchar(str1)),
#                           '4' = paste0(str1, '-01-01'),    # needed to get around R assigning current day and month when unspecified
#                           '7' = paste0(str1, '-01'),  # %Y-%d which doesn't work with strptime
#                           str1))
#   if (match.date != 'exact')
#     d2 <- year(d2)  
#   
#   match.pos <- sapply(x, CompareDates, date2=d2, compare.op =comp.op)
#   if (sum(match.pos) == 0){
#     print('No Matches')
#     return()
#   }
#   
#   return(match.pos)
# }

# UTF-8 compatible
MatchName <- function(nom, pattern, match.author=.BibOptions$match.author){
  if (is.null(nom))
    return(FALSE)
  if (match.author == 'exact'){
    nom <- as.character(nom)
  }else if (match.author == 'last.with.initials'){
    nom <- sapply(nom, function(x) paste0(paste0(substring(x$given, 1, 1), collapse = ''), x$family))
  }else{
    nom <- nom$family
  }
  return(all(pattern %in% nom))
}

# CompareEntries <- function(entry, searchterms, exact = FALSE, unicode=TRUE){
#   if(is.null(entry))
#     return(FALSE)
#   
#   if(!exact){
#     entry <- tolower(entry)
#     searchterms <- tolower(searchterms)
#   }
#   #fun <- ifelse(exact, 'match', 'pmatch')
#   
#   return(all(match(x=searchterms, table=entry, nomatch=FALSE)))
# }

# make UTF-8 compatible
# does not work: setGeneric('search', signature=)
# SearchField <- function(x, field, pattern){
#   # this code loses correct position of matches
#   # match(unlist(eval(parse(text=paste0('x$', field)))), pattern, nomatch=FALSE)
#  # browser()
#   match.pos <- sapply(do.call('$', list(x = x, name = field)), CompareEntries, searchterms=pattern, 
#                       exact = FALSE, unicode=TRUE)
#   return(match.pos)
# }

# TO Do: x['keyval']
`[.BibEntry` <- function(x, i, j, ..., drop =TRUE){
  # i is character vector
  # i is numeric
  # i is logical
  # i is list
  # i is missing
  if (!length(x) || (missing(i) && missing(...))) 
    return(x)
  if (missing(i)){
    if (missing(...))
      return(x)
    dots <- list(...)
  }else if (is.numeric(i) || is.logical(i)){  # obvious indices
    return(x[[i]])
  }else if (is.character(i)){
    if (is.null(names(i)))  # assume keys
      return(x[[i]])
    dots <- as.list(i)  # names correspond to fields, value to search terms
  }else if (is.list(i)){
    dots <- i
  }else{
    stop('Invalid index.')
  }
  
  fields <- names(dots)
  for (i in seq_along(dots))
    x <- x[FindBibEntry(x, dots[[i]], fields[i])]
  if (!length(x)){
    message("No results.")
    return()
  }
  # class(x) <- c("BibEntry", "bibentry")
  return(x)    
  # current.fields <- c(unique(names(unlist(x))), 'bibtype', 'key')    
}

FindBibEntry <- function(bib, term, field){
  vals <- do.call('$', list(x = bib, name = field))
  if (!length(unlist(vals))){
    res <- logical(length(bib))
  }else if (field %in% .BibEntryNameList){
    term <- ArrangeAuthors(term)
    match.aut <- .BibOptions$match.author
    if (match.aut == 'exact'){
      term <- as.character(term)
    }else if (match.aut == 'last.with.initials'){
      term <- sapply(term, function(x) paste0(paste0(substring(x$given, 1, 1), collapse = ''), x$family))
    }else{
      term <- term$family
    }
    res <- sapply(vals, MatchName, pattern = term, match.author = match.aut)
  }else if (field %in% .BibEntryDateField){
    if (field == 'month'){
      res <- sapply(vals, pmatch, table = term, nomatch = FALSE)
    }else{  
      if (field %in% c('date', 'year')){
        vals <- do.call('$', list(x = bib, name = 'dateobj'))
        match.dat <- ifelse(field == 'year', 'year.only', .BibOptions$match.date)
      }else{  # eventdate, origdate, urldate
        vals <- lapply(vals, function(x){
          res <- try(ProcessDate(x), TRUE)
          if (inherits(res, 'try-error'))
            return()
          res
        })
        match.dat <- .BibOptions$match.date
      }
      term <- try(ProcessDate(term, NULL, TRUE))
      if (is.null(term) || inherits(term, 'try-error')){
        res <- logical(length(bib))
      }else{    
        res <- sapply(vals, MatchDate, pattern = term, match.date = match.dat)
      }
    }
  }else if (field == "dateobj"){
    if (.BibOptions$match.date == 'year.only'){
      vals <- lapply(vals, year)
      term <- year(term)
    }
    res <- sapply(vals, `==`, term)
    res[sapply(temp, length)==0] <- FALSE
  }else{
    res <- sapply(vals, pmatch, table = term, nomatch = FALSE)
  }
  res
}

#       
#   if (!missing(j) && !missing(...)){
#     dots <- list(i, j, ...)
#   }else if (!missing(j)){
#     dots <- list(i, j)
#   }else{
#     if (!is.list(i))
#       dots <- list(i)
#   }
#       
#   return.ind <- .BibOptions$return.ind 
# 
#   current.fields <- c(unique(names(unlist(x))), 'bibtype', 'key')
#      # browser()
#   if(length(dots)==1){
#     dot.arg <- dots[[1]]
# 
#     if(is.numeric(dot.arg) || is.logical(dot.arg)){  # simple subsetting
#       return(x[[dot.arg]])
#     }else if (is.list(dot.arg)){  # call "[" again with unlisted args
#       args <- as.list(sapply(1:length(dot.arg), function(i) c(names(dot.arg)[i], dot.arg[[i]])))
#       args$x <- x
#       return(do.call('[.BibEntry', args))
#     }else if (is.character(dot.arg)){  
#       dot.arg <- tolower(dot.arg)
#       #browser()
#       if (dot.arg %in% current.fields){
#         res <- do.call("$", list(x = x, name = dot.arg))
#         if (dot.arg == 'author' || dot.arg == 'editor'){
#           rnames <- names(x)
#           y <- NULL
#           for (i in seq_along(res)){
#             temp <- as.character(res[[i]])
#             names(temp) <- paste0(rnames[i], 1L:length(temp))
#             y <- c(y, temp)
#           }
#           return(setNames(as.person(y), names(y)))
#         }else{
#           names(res) <- names(x)
#           return(unlist(res))
#         }
#           res <- sapply(setNames)
#         res <- unlist(res)
#         return(res)
#       }else{  # assumed to be keys
#         res <- x[[names(x) %in% dot.arg]]
#         if (length(res)==0)
#           message('No results.')
#         return(res)
#       }
#     }else{
#       stop('Invalid argument')
#     }
#   }
#   # dots$x <- NULL
#   # browser()
#   temp <- dots[[1]]
# #   if (is.numeric(temp)){ # simple subset use bibentry's "["
# #     print('WOW YOU FOUND ME!!!')
# #     class(x) <- 'bibentry'
# #     dots <- dots[-1]
# #     dots$x <- x[temp]
# #     class(dots$x) <- c('BibEntry', 'bibentry')
# #   }else 
#   if (pmatch(temp <- tolower(temp), current.fields, nomatch=0)){
# #     if(length(dots)==1){ 
# #       print('WOW YOU FOUND THE OTHER ME!!!')
# #       dots$x <- eval(parse(text=paste0('x$', temp)))
# #     }else{
#       pattern <- dots[[2]]
#       
#       if (temp=='author' || temp=='editor'){  # need special handling for a/e and y/d
#         match.pos <- MatchAuthor(x, field = temp, pattern = pattern)
#       }else if (temp == 'year' || temp == 'date'){
#         match.pos <- MatchDate(x, pattern = pattern)
#       #  }else if (temp == 'bibtype'){
#       #  if(length(pattern) > 1)
#       #    stop('Search term for bibtype must be length one since each entry can have only one type')
#       #  dots$x <- x[as.logical(mapply(function(x, table) match(tolower(attr(unclass(x)[[1]], 'bibtype')), 
#       #                                            pattern, nomatch=FALSE ), x=x, table=pattern))]
#       }else{
#         match.pos <- SearchField(x, field=temp, pattern=pattern)
#       }
#       dots <- dots[-c(1, 2)]
# #    }    
#   }else{
#     stop('Invalid argument')
#   }
#   
#   if(sum(match.pos)==0){
#     message('No matches')
#     return()
#   }
#   
#   if (length(dots) > 1){ # perform recursion
#     dots$x <- x[match.pos]
#     dots$x <- do.call("[.BibEntry", dots) 
#   }
#   if(return.ind){
#     return(which(match.pos))
#   }else{
#     x <- x[match.pos]
#     class(x) <- c('BibEntry', 'bibentry')
#     return(x)  
#   }
#   
# }
