ReadBib <- function (file = findBibFile(package), package = "bibtex", encoding = "UTF-8", 
                     header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
                     footer = "") 
{
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
                                                               encoding = encoding))
  out <- .External("do_read_bib", file = file, encoding = encoding, 
                   srcfile = srcfile)
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, MakeBibEntry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- MakeCitationList(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  out
}

MakeBibEntry <- function (x) {
  type <- attr(x, "entry")
  key <- attr(x, "key")
  y <- as.list(x)
  names(y) <- tolower(names(y))
  if ("author" %in% names(y)) {
    y[["author"]] <- ArrangeAuthors(y[["author"]])
  }
  if ("editor" %in% names(y)) {
    y[["editor"]] <- ArrangeAuthors(y[["editor"]])
  }
  
  if("date" %in% names(y)){
    y[['date']] <- as.Date(switch(as.character(nchar(y[['date']])),
                                  '4' = paste0(y[['date']], '-01-01'),    # needed to get around R assigning current day and month when unspecified
                                  '7' = paste0(y[['date']], '-01'),  # %Y-%d which doesn't work with strptime
                                  y[['date']]))
  }else if ("year" %in% names(y)){
    y[["date"]] <- as.Date(paste0(y[["year"]], '-01-01'))
  }
  
  tryCatch(BibEntry(bibtype = type, key = key, other = y), 
           error = function(e) {
             message(sprintf("ignoring entry '%s' (line %d) because :\n\t%s\n", 
                             key, attr(x, "srcref")[1], conditionMessage(e)))
             NULL
           })
}