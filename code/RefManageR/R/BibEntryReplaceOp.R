`[<-.BibEntry` <- function(x, i, j, ..., value){
  if (missing(i)){
    y <- x
  }else if (missing(j)){
    y <- x[i]
    
  }else{
    y <- x[i, j, ...]
  }
  if (!length(y))
    stop('Object to replace has length 0, bad index specified.')
  
  names.to.replace <- names(y)
  N.to.replace <- length(y)
  y <- unclass(y)
  if (!N.to.replace)
    stop('No elements to replace.')
 
  if (inherits(value, 'bibentry')){
   
    N.replacements <- length(value)
    value <- unclass(value)
    
    ind <- rep_len(1L:N.replacements, N.to.replace)
    if (N.to.replace%%N.replacements != 0L)
      warning('Number of items to replace is not a multiple of replacement length.')
#     if(!inherits(y, 'BibEntry'))
#       stop('Replacement is BibEntry Object, object to replace is not')
    y <- value[ind]
    
#  }else if (is.character(value)){  # only one replacement object
#    N.to.replace <- 1
#    y <- list(BibReplace(orig = y[[1]], replace.vals = value))
  }else if (is.character(value) || is.list(value)){
    if(is.character(value))
      value <- list(value)
    N.replacements <- length(value)
    ind <- rep_len(1L:N.replacements, N.to.replace)
    if (N.to.replace%%N.replacements != 0L)
      warning('Number of items to replace is not a multiple of replacement length.')
#    y <- lapply(y, BibReplace, replace.vals = value) 
#     .fields <- names(value)
#     if (is.null(.fields) || any(.fields == ''))
#       stop('All values in replacement must have a name corresponding to BibTeX field.')
  #  browser()
    for (i in seq_along(y))
      y[[i]] <- BibReplace(y[[i]], value[[ind[i]]])
   # y <- mapply(BibReplace, y, setNames(value[ind], names(value)[ind]), SIMPLIFY = FALSE)
    #y <- mapply(function(o, r) BibReplace(o, r, names(r)), y, value[ind], SIMPLIFY = FALSE)
    
  }else{
    stop('Object for replacement should be of class list, character, or BibEntry')
  }
#   if (!is.null(value) && N %% length(x) == 0){
#     warning(paste0('BibEntry object length is not a multiple of replacement length'))
#   }
#  browser()
  replace.ind <- match(names.to.replace, names(x))
  x <- unclass(x)

  for (k in seq_len(N.to.replace))
    x[[replace.ind[k]]] <- y[[k]]
  
  class(x) <- c('BibEntry', 'bibentry')
  return(x)
}

BibReplace <- function(orig, replace.vals){
 # browser()
  replace.fields <- names(replace.vals)
  if (is.null(replace.fields) || any(replace.fields == ''))
    stop('Replacement object must have names corresponding to fields')
  if ('key' %in% replace.fields){
    attr(orig, 'key') <- replace.vals[['key']]
    if (length(replace.vals) > 1){
      replace.vals[['key']] <- NULL
    }else{
      return(orig)
    }
  }
  if ('bibtype' %in% replace.fields){
    attr(orig, 'bibtype') <- replace.vals[['bibtype']]
    if (length(replace.vals) > 1){
      replace.vals[['bibtype']] <- NULL
    }else{
      return(orig)
    }
  }
 # browser()
  nl.to.update <- replace.fields %in% .BibEntryNameList
  for (i in replace.fields[nl.to.update])
    orig[i] <- ArrangeAuthors(i)
#   if (any(nl.to.update)){
#     #tmp <- orig
#     orig[replace.vals[nl.to.update]] <- sapply(replace.vals[nl.to.update], ArrangeAuthors)
#   }
#   if ('author' %in% replace.fields){
#     orig[['author']] <- as.person(replace.vals[['author']])
#   }
#   if ('editor' %in% replace.fields)
#     orig[['editor']] <- as.person(replace.vals[['editor']])
#   df.to.update <- replace.fields %in% .BibEntryDateField
#   if (any(df.to.update)){
#     for (i in which(df.to.update)){
#       if (replace.fields[i] == 'month'){
#         orig[['month']] <- replace.vals[i]
#       }else{
#         
#       }
#       
#     }
#     if (!inherits(replace.vals[['date']], "POSIXlt"))
#       orig[['date']] <- as.Date(switch(as.character(nchar(replace.vals[['date']])),
#                                   '4' = paste0(replace.vals[['date']], '-01-01'),    # needed to get around R assigning current day and month when unspecified
#                                   '7' = paste0(replace.vals[['date']], '-01'),  # %Y-%d which doesn't work with strptime
#                                   replace.vals[['date']]))
#     orig[['year']] <- as.Date(paste0(year(replace.vals[['date']]), '-01-01'))
#   }
#   if ("year" %in% replace.fields){
#     orig[['year']] <- as.Date(paste0(replace.vals[['year']], '-01-01'))
#     orig[['date']] <- as.Date(paste(replace.vals[['year']], month(orig$date), day(orig$date), sep='-'))
#   }
 # browser()
  replace.remains <- replace.vals[!replace.fields %in% c('bibtype', 'key', .BibEntryNameList)]
  if (length(replace.remains)){
    replace.names <- names(replace.remains)
    for (i in seq_along(replace.remains))
      orig[[replace.names[i]]] <- replace.remains[i]
  }
  if (any(replace.fields %in% .BibEntryDateField)){  # update dateobj attribute
    tdate <- try(ProcessDates(orig), TRUE)
    if (inherits(tdate, 'try-error') || is.null(tdate) || is.na(tdate))
      stop(paste0('The specified Date Field value is not in a valid format for Bibtex/Biblatex'))
    attr(orig, 'dateobj') <- tdate
  }

  return(orig)
}