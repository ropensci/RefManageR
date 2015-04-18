#' Create document from markdown using pandoc
#'
#' Convert RMarkdown documents to other formats using \code{knitr} and \code{pandoc}
#' allowing for the use of CSL styles
#' @param infile character; file name of an RMarkdown document to be converted
#' @param outfile character; file name to write output to, including the extension to
#' specify the desired file type
#' @param csl character; citation style language style to use to generate the bibliography;
#' see the references.
#' @param pandoc.opts character; additional options to pass to pandoc
#' @importFrom knitr knit
#' @references \url{http://johnmacfarlane.net/pandoc/README.html}
#' @references \url{http://citationstyles.org/downloads/primer.html}
RenderWithPandoc <- function(infile, outfile, csl="ieee.csl", pandoc.opts = ""){
  pandocExists <- system("pandoc -v", ignore.stdout = TRUE, ignore.stderr = TRUE,
                         show.output.on.console = FALSE) == 0
  pandocPath <- if (pandocExists)
                  "pandoc"
                else{
                  temp <- readline(prompt = paste0(
                    "Please specify the path to the pandoc executable",
                                       options("prompt")))
                  if (!length(grep("pandoc$", temp)))
                    temp <- paste(temp, "pandoc", sep = "/")
                  if (suppressWarnings(system2(temp, "-v", stdout = FALSE,
                         stderr = FALSE) != 0))
                    stop("pandoc not found.")
                  temp
                }
  pandoc.opts <- unlist(strsplit(pandoc.opts, " "))
  knit.outfile <- sub("[.]Rmd$", ".md", infile)
  knit(infile, knit.outfile)
  doc <- readLines(knit.outfile)
  m <- gregexpr("(^|[[:space:][])@([[:alpha:]_][[:alnum:]:.#$%&-+?<>~/]*)", doc)
  citations <- unlist(regmatches(doc, m))
  citations <- sub("(^|[[:space:][])@", "", citations)
  ## citations <- unlist(lapply(citations, function(x) if (length(x)) x[3]))
  tfile <- tempfile(fileext = ".bib")
  WriteBib(bib, tfile, biblatex = TRUE)

  system2(pandocPath, args = c(pandoc.opts, "--biblio", "tfile", "--csl", csl, "-f",
                           knit.outfile, "-o", outfile))
  unlink(tfile)
}
