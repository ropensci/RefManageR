#' @keywords internal
.onLoad <- function(lib, pkg){
  library.dynam("bibtex", "bibtex", "C:/Users/Matthew/Documents/R/win-library/3.1") 
  setOldClass("BibEntry")
  
}

# #' @keywords internal
# .onAttach <- function(libname, pkgname){
#   try(options(encoding = "UTF-8"), TRUE)
# }
