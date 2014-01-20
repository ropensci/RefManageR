as.BibEntry <- function(x){
  if (inherits(x, 'BibEntry')){
    class(x) <- c('BibEntry', 'bibentry')
  }else if (inherits(x, 'bibentry')){
    att <- attributes(x)
    x <- lapply(unclass(x), function(y){
      attr(y, 'dateobj') <- ProcessDates(y)
      check <- try(.BibEntryCheckBibEntry1(y), TRUE)
      if (inherits(check, 'try-error')){
        message(paste0('Ignoring entry titled \"', y[['title']], '\" because ', strsplit(check, '\\n[[:space:]]*')[[1]][2]))
        return(NULL)
      }
      y
    })
    x <- x[!sapply(x, is.null)]
    if (length(x)){
      attributes(x) <- att
      class(x) <- c('BibEntry', 'bibentry')  
    }
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
    if(length(x) == 1L){
      if (!is.null(attr(x, 'bibtype'))){  # x simply unclass'ed
        class(x) <- c('BibEntry', 'bibentry')
      }else if (!is.null(x$dateobj)){  # x has been unlist'ed
        x <- relist.BibEntry(x)
      }else{  # user supplied list
        x <- do.call('BibEntry', x)
      }
    }else{
      if (!is.null(attr(x[[1L]], 'bibtype'))){  # x simply unclass'ed
        class(x) <- c('BibEntry', 'bibentry')
      }else if (!is.null(x[[1L]]$dateobj)){
        x <- relist.BibEntry(x)
      }else{
        x <- sapply(x, function(...) do.call(BibEntry, ...))
        class(x) <- c('BibEntry', 'bibentry')            
      }
    }
  }else{
    stop(paste0("Cannot coerce object of class '", class(x), "' to BibEntry"))
  }
  return(x)
}