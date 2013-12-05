as.data.frame.BibEntry <- function(x, ...){
  col.names <- unique(unlist(fields(x)))
  n.fields <- length(col.names)
  y <- matrix(nr = length(x), nc = n.fields + 1)
  colnames(y) <- c('bibtype', col.names)
  rownames(y) <- names(x)
  browser()
  authors <- sapply(x, function(z) paste0(z$author, collapse = ', '))
  
  y[, 1] <- x['bibtype']
  for (i in 1:n.fields){
    nom <- col.names[i]
    if (nom != 'author'){
      temp <- x[nom]
    }else{
      temp <- authors
    }
    y[names(temp), col.names[i]] <- temp
  }
  
  y <- as.data.frame(y, stringsAsFactors = FALSE)
  return(y)
}