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
