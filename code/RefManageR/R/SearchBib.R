SearchBib <- function(x, ..., match.date = 'yearonly', match.author='lastonly'){
  
  # opt.def <- BibOptions()
  # BibOptions()$match.date <- match.date
  # BibOptions()$match.author <- match.author
  fcall <- match.call()
  fcall$return.ind <- TRUE
  fcall[[1L]] <- as.name('[.BibEntry')
  # BibOptions() <- opt.def
  return(eval(fcall))
}