# Mathew McLean
# January 15, 2014
# Define addition operator for objects of BibEntry class
# Note: will not remove duplicates entries already present in e1 or e2

`+.BibEntry` <- function(e1, e2, fields.to.check = .BibOptions$merge.fields.to.check){
  awl <- "all" %in% fields.to.check
  if (length(fields.to.check) && !awl){
    possible.dup <- seq_along(e2) 
    temp <- c("key", "bibtype") %in% fields.to.check
    if (all(temp)){
      possible.dup <- which(sapply(e2, 
                                    function(x, y){
                                      ykeys <- unlist(y$key)
                                      ytypes <- unlist(y$bibtype)
                                      xkey <- x$key
                                      xtype <- x$bibtype
                                      for (i in seq_along(y)){
                                        if (ykeys[i] == xkey && ytypes[i] == xtype)
                                          return(TRUE)
                                      }
                                      return(FALSE)
                                    }, y = e1))
    }else if (temp[1]){
      possible.dup <- which(names(e2) %in% names(e1))
    }else if (temp[2]){
      possible.dup <- unlist(e2$bibtype) %in% unlist(e1$bibtype)
    }
    remain.dup.f <- setdiff(fields.to.check, c('bibtype', 'key'))
    if (length(remain.dup.f)){
      dup.ind <- which(sapply(unclass(e2[possible.dup]), 
                      function(x, y, flds){
                        x <- x[flds]
                        for (i in seq_along(y)){
                          if (identical(y[[i]][flds], x))
                            return(TRUE)
                        }
                        return(FALSE)
                      }, y = unclass(e1), flds = remain.dup.f))
    }else{
      dup.ind <- possible.dup
    }
    if (length(dup.ind) == length(e2)){
      message('Only duplicates in second BibEntry object')
      return(e1)
    }
    if (length(dup.ind)){
      e2 <- e2[-dup.ind]
      message(paste0('Duplicate entries found in second BibEntry object in position(s): ', 
                     paste0(dup.ind, collapse = ', ')))
    }
  }
  x <- c(e1, e2)
  if (awl){
    x <- x[!duplicated(x)]  
  }
  names(x) <- make.unique(names(x), sep = ":")

  return(x)
}

merge.BibEntry <- function(x, y, fields.to.check = .BibOptions$merge.fields.to.check)
  UseMethod('+')