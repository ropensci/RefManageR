# Mathew McLean
# October 27, 2013
# Define addition operator for objects of BibEntry class
# Note: will not remove duplicates entries already present in e1 or e2

`+.BibEntry` <- function(e1, e2, duplicate.fields = .BibOptions$duplicate.fields){
  dup.ind <- match(names(e2), names(e1), nomatch=FALSE)
  good.ind <- 1:length(e2)  # need to print indices of removed 

  message(paste0('Repeated keys in ', deparse(substitute(e2)), ' for entries'))
  message(paste0(dup.ind, ' '))
  if('key' %in% duplicate.fields){
    message('These entries will be dropped in result')
    duplicate.fields <- duplicate.fields[duplicate.fields != 'key']
    e2 <- e2[-dup.ind]
    good.ind <- good.ind[-dup.ind]
  }else{
    message('Adding \'2\' to end of these keys in result')
    names(e2)[dup.ind] <- paste0(names(e2)[dup.ind],'2')
    dup.ind <- NULL
  }
  #browser()
  curopt <- .BibOptions$return.ind
  .BibOptions$return.ind <- TRUE
  current.fields <- c(unique(c(names(unlist(e1)), names(unlist(e2)))), 'bibtype')
  if(length(duplicate.fields)){
    for (i in 1:length(duplicate.fields)){
      curfield <- duplicate.fields[i]
      if(curfield %in% current.fields){
        cur.ind <- unlist(lapply(eval(parse(text=paste0('e2$', curfield))), function(x) suppressMessages(e1[curfield, x])))
        dup.ind <- c(dup.ind, cur.ind)
        if (length(dup.ind) == length(e2)){
          message(paste0('Only duplicates in ', deparse(substitute(e2))))
          return(e1)
        }
        e2 <- e2[-cur.ind]
        message(paste0('Repeated entries found in ', deparse(subsitute(e2)), ' when examining ', curfield, 'field for entries'))
        message(paste0(good.ind[cur.ind], ' '))
        good.ind <- good.ind[-cur.ind]
      }else{
        paste0(curfield, ' is not a field in any entries.  Ignoring.')
      }
    }
  }
  .BibOptions$return.ind <- curopt
  c(e1)
}