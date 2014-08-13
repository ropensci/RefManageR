\name{RenderWithPandoc}
\alias{RenderWithPandoc}
\title{Create document from markdown using pandoc}
\usage{
RenderWithPandoc(infile, outfile, csl = "ieee.csl", pandoc.opts = "")
}
\arguments{
  \item{infile}{character; file name of an RMarkdown
  document to be converted}

  \item{outfile}{character; file name to write output to,
  including the extension to specify the desired file type}

  \item{csl}{character; citation style language style to
  use to generate the bibliography; see the references.}

  \item{pandoc.opts}{character; additional options to pass
  to pandoc}
}
\description{
Convert RMarkdown documents to other formats using
\code{knitr} and \code{pandoc} allowing for the use of CSL
styles
}
\references{
\url{http://johnmacfarlane.net/pandoc/README.html}

\url{http://citationstyles.org/downloads/primer.html}
}

