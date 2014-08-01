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
#' @references \url{http://johnmacfarlane.net/pandoc/README.html}
#' @references \url{http://citationstyles.org/downloads/primer.html}
#'
RenderDoc <- function(infile, outfile, csl="ieee.csl", pandoc.opts = ""){
  pandocExists <- system("pandoc -v", ignore.stdout = TRUE, ignore.stderr = TRUE,
                         show.output.on.console = FALSE) == 0
  pandocPath <- if (pandocExists)
                  "pandoc"
                else
                  readline(prompt = "Please specify the path to pandoc executable.")
  pandoc.opts <- unlist(strsplit(pandoc.opts, " "))
  knit.outfile <- gsub("[.]Rmd$", "[.]md", infile)
  knit(infile, knit.outfile)
  tfile <- tempfile(fileext = ".bib")
  WriteBib(bib, tfile, biblatex = TRUE)


  system2(pandocPath, args = c(pandoc.opts, "--biblio", "tfile", "--csl", csl, "-f",
                          knit.outfile, "-o", outfile))
  unlink(tfile)
}
