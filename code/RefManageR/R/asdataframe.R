as.data.frame.BibEntry <- function(x, ...){
  col.names <- unique(unlist(fields(x)))
  n.fields <- length(col.names)
  y <- matrix(nr = length(x), nc = n.fields + 1L)
  colnames(y) <- c('bibtype', col.names)
  rownames(y) <- names(x)
#   browser()
#   authors <- x$author
#   names(authors) <- names(x)
#   authors <- sapply(authors, paste0, collapse =', ')
  #authors <- setNames(sapply(x, function(z) paste0(z$author, collapse = ', '), USE.NAMES = TRUE), rownames(y))
       
  y[, 1] <- x['bibtype']
  for (i in seq_len(n.fields)){
    nom <- col.names[i]
    if (nom != 'author' && nom != 'editor'){
      temp <- x[nom]
    }else{
      temp <- do.call(`$`, list(x = x, name = nom))  # eval(parse(text=paste0('x$', nom)))
      temp <- sapply(setNames(temp, rownames(y)), function(z) paste0(z, collapse = ', '))
    }
    y[names(temp), col.names[i]] <- temp
  }
  
  y <- as.data.frame(y, stringsAsFactors = FALSE)
  return(y)
}