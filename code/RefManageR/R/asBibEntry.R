as.BibEntry <- function(x){
  if (inherits(x, c('BibEntry', 'bibentry'))){
    class(x) <- c('BibEntry', 'bibentry')
  }else if (is.character(x)){
    
    if (is.na(x['bibtype']) || is.na(x['key']))
      stop("Object of class character must have entries named bibtype and key.")
    x <- as.list(x)
    attr(x, 'entry') <- x$bibtype
    attr(x, 'key') <- x$key
    x$bibtype <- NULL
    x$key <- NULL
    x <- MakeBibEntry(x, FALSE)
  }else if(is.data.frame(x)){
    .fields <- colnames(x)
    if (is.null(x$bibtype))
      stop("data.frame must have column for 'bibtype'.")
    keys <- rownames(x)
    if (keys[1] == '1')
      warning('rownames of data.frame not meaningful for creating keys')

    y <- vector('list', length(x))
    for (i in 1L:nrow(x)){
      na.ind <- which(!is.na(x[i, ]))
      y[[i]] <- as.BibEntry(c(setNames(as.character(x[i, na.ind]), .fields[na.ind]), key = keys[i]) )
    }
    y <- MakeCitationList(y)
    return(y)

  }else if(is.list(x)){
    x <- relist.BibEntry(x)
  }else{
    stop(paste0("Cannot coerce object of class '", class(x), "' to BibEntry"))
  }
  return(x)
}