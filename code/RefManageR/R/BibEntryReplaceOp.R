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
  browser()
  if (inherits(value, 'bibentry')){
    value <- unlist(value)
    N.replacements <- length(value)
    if (N.to.replace%%N.replacements != 0L)
      warning('Number of items to replace is not a multiple of replacement length.')
#     if(!inherits(y, 'BibEntry'))
#       stop('Replacement is BibEntry Object, object to replace is not')
    y <- value[ind]
    
  }else if (is.character(value)){
    y <- lapply(y, BibReplace, replace = value)
  }else if (is.list(value)){
    value <- as.list(value)
    N.replacements <- length(value)
    ind <- rep_len(1L:N.replacements, N.to.replace)
    if (N.to.replace%%N.replacements != 0L)
      warning('Number of items to replace is not a multiple of replacement length.')
    
    .fields <- names(value)
    if (is.null(.fields) || any(.fields == ''))
      stop('All values in replacement must have a name corresponding to BibTeX field.')
  #  browser()
    for (i in seq_along(y))
      y[[i]] <- BibReplace(y[[i]], value[ind[i]])
   # y <- mapply(BibReplace, y, setNames(value[ind], names(value)[ind]), SIMPLIFY = FALSE)
    #y <- mapply(function(o, r) BibReplace(o, r, names(r)), y, value[ind], SIMPLIFY = FALSE)
    
  }else{
    stop('Object for replacement should be of class list, character, or BibEntry')
  }
#   if (!is.null(value) && N %% length(x) == 0){
#     warning(paste0('BibEntry object length is not a multiple of replacement length'))
#   }
  
  replace.ind <- match(names.to.replace, names(x))
  for (k in seq_along(y))
    x[[replace.ind[k]]] <- y[k]
  
  class(x) <- c('BibEntry', 'bibentry')
  return(x)
}

BibReplace <- function(orig, replace){
  replace.fields <- names(replace)
  if (is.null(replace.fields))
    stop('Replacement object must have names corresponding to fields')
  if ('key' %in% replace.fields){
    attr(orig, 'key') <- replace[['key']]
    replace[['key']] <- NULL
  }
  if ('bibtype' %in% replace.fields){
    attr(orig, 'bibtype') <- replace[['bibtype']]
    replace[['bibtype']] <- NULL
  }

  if ('author' %in% replace.fields)
    replace[['author']] <- as.person(replace[['author']])
  if ('editor' %in% replace)
    replace[['editor']] <- as.person(replace[['editor']])
  if ("date" %in% replace.fields){
    if (!inherits(replace[['date']], "POSIXlt"))
      replace[['date']] <- as.Date(switch(as.character(nchar(replace[['date']])),
                                  '4' = paste0(replace[['date']], '-01-01'),    # needed to get around R assigning current day and month when unspecified
                                  '7' = paste0(replace[['date']], '-01'),  # %Y-%d which doesn't work with strptime
                                  replace[['date']]))
    replace[['year']] <- as.Date(paste0(year(replace[['date']]), '-01-01'))
    if (!'year' %in% replace.fields)
      replace.fields <- c(replace.fields, 'year')
  }
  if ("year" %in% replace.fields){
    replace[['year']] <- as.Date(paste0(replace[['year']], '-01-01'))
    replace[['date']] <- as.Date(paste(replace[['year']], month(orig$date), day(orig$date), sep='-'))
    if (!'date' %in% replace.fields)
      replace.fields <- c(replace.fields, 'date')
  }

  if (length(replace))
    orig[replace.fields] <- replace
  return(orig)
}