#' Update Different Fields of Multiple Entries of a BibEntry Object
#' 
#' Assign new values for specified fields in a BibEntry object using a named
#' character vector or list of named character vectors.
#' @param x - a BibEntry object.
#' @param i - see \code{\link{[.BibEntry}}
#' @param j - see \code{\link{[.BibEntry}
#' @param ... - see \code{\link{[.BibEntry}
#' @param value - values to be assigned to \code{x}.  To update one entry only,
#' should be a named character vector with names corresponding to fields.  To update
#' multiple entries, should be a list of named character vectors.  Can also be an object of 
#' class BibEntry.
#' @return an object of class BibEntry.
#' @note Date and name list fields should be in the format expected
#' by Biblatex (see \code{\link{BibEntry}}).
#' @S3method [<- BibEntry
#' @keywords methods manip
#' @seealso \code{\link{[.BibEntry}}, \code{\link{$<-.BibEntry}}, \code{\link{[[<-.BibEntry}}
#' @examples
#' file.name <- system.file("sampleData", "RJC.bib", package="RefManageR")
#' bib <- ReadBib(file.name)
#' print(bib[seq_len(3L)], .opts = list(sorting = "none", .bibstyle = "alphabetic"))
#' ## add month to Serban et al., add URL and urldate to Jennings et al., and
#' ##   add DOI and correct journal to Garcia et al.
#' bib[seq_len(3L)] <- list(c(date="2013-12"), 
#'                         c(url="http://bsb.eurasipjournals.com/content/2013/1/13", 
#'                           urldate = "2014-02-02"), 
#'                         c(doi="10.1093/bioinformatics/btt608", 
#'                           journal = "Bioinformatics")) 
#' print(bib[seq_len(3L)], .opts = list(sorting = "none", bib.style = "alphabetic"))
#' bib2 <- bib[seq_len(3L)]
#' bib2[2:3] <- bib[5:6]
#' bib2
#' bib2[3] <- c(journal='', eprinttype = "arxiv", eprint = "1308.5427", eprintclass = "math.ST",
#'                            pubstate = "submitted", bibtype = "misc")
#' bib2                            
`[<-.BibEntry` <- function(x, i, j, ..., value){
  if (!length(value))
    return(x)
  
  ret.ind <- .BibOptions$return.ind
  .BibOptions$return.ind <- TRUE
  kal <- match.call(expand.dots = TRUE)
  kal$value <- NULL
  kal[[1]] <- `[.BibEntry`
  ind <- eval(kal)
  y <- x[[ind]]
  
#   if (missing(i)){
#     y <- x
#   }else if (missing(j)){
#     ind <- x[i]
#     y <- x[[ind]]
#   }else{
#     ind <- x[i, j, ...]
#     y <- x[[ind]]
#   }
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
    
    ind <- rep_len(seq_len(N.replacements), N.to.replace)
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
  #browser()
  replace.ind <- match(names.to.replace, names(x))
  x <- unclass(x)
  for (k in seq_len(N.to.replace))
    x[[replace.ind[k]]] <- y[[k]]
  
  class(x) <- c('BibEntry', 'bibentry')
  .BibOptions$return.ind <- ret.ind
  return(x)
}

BibReplace <- function(orig, replace.vals){
  replace.fields <- names(replace.vals)
  if (is.null(replace.fields) || any(replace.fields == ''))
    stop('Replacement object must have names corresponding to fields')
  if ('key' %in% replace.fields){
    attr(orig, 'key') <- replace.vals[['key']]
#     if (length(replace.vals) > 1){
#       replace.vals[['key']] <- NULL
#     }else{
#       return(orig)
#     }
  }
  if ('bibtype' %in% replace.fields){
    BibLaTeX_names <- names(BibLaTeX_entry_field_db)
    pos <- match(tolower(replace.vals[['bibtype']]), tolower(BibLaTeX_names))
    if (is.na(pos))
      stop('Invalid bibtype specified')
    attr(orig, 'bibtype') <- BibLaTeX_names[pos]
#     browser()
#     if (length(replace.vals) > 1){
#       replace.vals[['bibtype']] <- NULL
#     }else{
#       return(orig)
#     }
  }
 # browser()
  nl.to.update <- replace.fields %in% .BibEntryNameList
  for (i in replace.fields[nl.to.update])
    orig[[i]] <- ArrangeAuthors(replace.vals[[i]])

  replace.remains <- replace.vals[!replace.fields %in% c('bibtype', 'key', .BibEntryNameList)]
  if (length(replace.remains)){
    replace.names <- names(replace.remains)
    for (i in seq_along(replace.remains)){
      if (nchar(replace.remains[i])){
        orig[[replace.names[i]]] <- replace.remains[i]  
      }else{
        orig[[replace.names[i]]] <- NULL
      }
    }
      
  }
  if (any(replace.fields %in% .BibEntryDateField)){  # update dateobj attribute
    tdate <- ProcessDates(orig)
    if (is.null(tdate))
      stop(paste0('The specified Date Field value is not in a valid format for Bibtex/Biblatex'))
    attr(orig, 'dateobj') <- tdate
  }

  return(orig)
}

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
#  browser()