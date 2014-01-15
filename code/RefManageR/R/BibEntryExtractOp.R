library(lubridate)

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
  keys <- names(x)  
  fields <- names(dots)
  add <- function(x) suppressMessages(Reduce("+", x))
  for (i in seq_along(dots))
    x <- add(lapply(dots[[i]], function(trm, bib, fld) x[FindBibEntry(bib, trm, fld)], bib = x, fld = fields[i]))  # x[FindBibEntry(x, dots[[i]], fields[i])]
  if (!length(x)){
    message("No results.")
    return(list())
  }
  # class(x) <- c("BibEntry", "bibentry")
  if (.BibOptions$return.ind){
    return(which(keys %in% names(x))) 
  }else{
    return(x)      
  }
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
    res[sapply(res, length)==0] <- FALSE
  }else{
    res <- logical(length(bib))
    for (i in seq_along(bib))
      res[i] <- as.logical(pmatch(vals[i], term, 0L))
#     res <- lapply(vals, pmatch, table = term)
#     res[sapply(res, length)==0] <- FALSE  # lame behaviour of nomatch necessitates this
#     res <- as.logical(unlist(res))
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
