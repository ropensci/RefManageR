#' @keywords internal
.onLoad <- function(lib, pkg){
  library.dynam("bibtex", "bibtex", "C:/Users/Matthew/Documents/R/win-library/3.1") 
  setOldClass("BibEntry")
}
