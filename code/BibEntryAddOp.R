# Mathew McLean
# October 27, 2013
# Define addition operator for objects of BibEntry class
# Note: will not remove duplicates entries already present in e1 or e2

`+.BibEntry` <- function(e1, e2, duplicate.check=.BibOptions$duplicate.check){
  dup.ind <- match(names(e2), names(e1), nomatch=FALSE)
  
  if (duplicate.check == 'keys.only'){
    return(c(e1, e2[!dup.ind]))
  }else if (duplicate.check == 'keys.and.title'){
    names()
    dup.ind <- as.logical(dup.ind+tdup.ind)
  }
}