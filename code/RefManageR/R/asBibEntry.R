as.BibEntry <- function(x){
  if (is.character(x)){
    
  }else if(is.list(x)){
    x <- relist.BibEntry(x)
  }else if(is.data.frame(x)){
    
  }else{
    stop("Cannot coerce '", match.call()$x, "' to BibEntry")
  }
  return(x)
}