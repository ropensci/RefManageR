

## opts_knit$set(self.contained = TRUE,
##              root.dir = '~/biblatex/code')
## opts_chunk$set(concordance = TRUE)
library(knitr)
# library(lubridate)
# lapply(list.files(pattern='.R$', full=TRUE), source, echo = FALSE, verbose = FALSE)
# lapply(list.files(path = "./TEMP-ROXY-DONE/", pattern='.R$', full=TRUE), source, echo = FALSE, verbose = FALSE)
library(RefManageR)

opts_knit$set(out.format = "latex")
# opts_chunk$set(cache=TRUE, autodep=TRUE)
knit_theme$set('fine_blue')
knit_hooks$set(message = function(x, options) {
    return(paste0('\\begin{lstlisting}\n',
      x,
    '\\end{lstlisting}'))
})
knit_hooks$set(error = function(x, options) {
    return(paste0('\\begin{lstlisting}[style=error]\n',
      x,
    '\\end{lstlisting}'))
})



library(tools, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
bib <- BibEntry(bibtype="Article", key = "barry1996", date = "1996-08", 
  title = "A Diagnostic to Assess the Fit of a Variogram to Spatial Data",
  author = "Ronald Barry", journaltitle = "Journal of Statistical Software",
                 volume = 1, number = 1)
bib
toBiblatex(bib)



bibentry("misc", key = "mclean2013bayesian", author = "M. W. McLean and 
         F. Scheipl and G. Hooker and S. Greven and D. Ruppert",
         title = "Bayesian Functional Generalized Additive Models 
                 with Sparsely Observed Covariates", year = "Submitted", 
         note = "arXiv eprint: 1305.3585")



BibEntry("misc", key = "mclean2013bayesian", author = "McLean, M. W. and 
         Scheipl, F. and Hooker, G. and Greven, S. and Ruppert, D.",
         title = "Bayesian Functional Generalized Additive Models 
                 with Sparsely Observed Covariates", urldate = "2013-10-06", 
         date = "2013", eprinttype = "arxiv", eprintclass = "stat.ME", 
         eprint = "1305.3585", pubstate = "submitted")



bib <- BibEntry(bibtype="XData", key = "statME", eprinttype = "arxiv", 
                eprintclass = "stat.ME")
bib <- c(bib, BibEntry(bibtype="XData", key = "online2013", year = "2013", 
                       urldate = "2013-12-20"))
toBiblatex(bib)
bib <- c(bib, BibEntry(bibtype="Online", key="mclean2013rlrt", 
  author = "Mathew McLean and Giles Hooker and David Ruppert",
  title = "Restricted Likelihood Ratio Tests for Scalar-on-Function Regression",
  eprint = "1310.5811", url = "http://arxiv.org/abs/1310.5811",
  xdata = "statME,online2013"))
bib <- c(bib, BibEntry(bibtype="Online", key="mclean2013bayesian", 
  author = paste("Mathew McLean and Fabian Scheipl and Giles Hooker",
                "and Sonja Greven and David Ruppert"), 
  title = paste("Bayesian Functional Generalized Additive Models", 
               "for Sparsely Observed Covariates"),
  eprint = "1305.3585", url = "http://arxiv.org/abs/1305.3585",
  xdata = "statME,online2013"))
bib



c(BibEntry("book", key = "parent", title = "The Book Title", year = 2012, 
           subtitle = "The Book Subtitle", author = "Book Author", 
           publisher = "A publisher"), BibEntry("inbook", key = "child", 
           crossref = "parent", title = "The Title of the In Book Entry", 
           author = "In Book Author"))



BibEntry(bibtype="Collection", key = "jaffe", editor = "Phillip Jaff\u00eb", 
  title = "Regesta Pontificum Romanorum ab condita ecclesia ad annum post
  Christum natum {MCXCVIII}", date = "1885/1888",
  editora = "S. Loewenfeld and F. Kaltenbrunner and P. Ewald",
  editoratype = "redactor", totalpages = "10", bookpagination = "section")



file <- system.file("Bib", "biblatexExamples.bib", package = "RefManageR")
bib <- ReadBib(file, check = "error")
bib <- ReadBib(file, check = FALSE)
print(bib[c("cms", "jcg", "ctan")], .opts = list(check.entries = FALSE))



## tmpdir <- tempdir()
## tmpfile <- tempfile(".zip", tmpdir)
## download.file("http://dl.dropbox.com/u/3291828/Poppler/poppler.0.22.0_win32.zip",
##               tmpfile)
## unzip(tmpfile, exdir = tmpdir)
## curdir <- getwd()
## setwd(file.path(tmpdir, "bin", fsep = "\\"))
## download.file("http://www.jstatsoft.org/v56/i11/paper", "jss.pdf",
##                 mode = "wb")
## download.file("http://arxiv.org/pdf/math/0703791",
##               destfile = "FIZaop.pdf", mode = "wb")
## download.file("http://arxiv.org/pdf/math/0703858",
##               destfile = "PBHTaos.pdf", mode = "wb")
## download.file("http://biomet.oxfordjournals.org/content/83/4/715.full.pdf",
##   destfile = "ADVb.pdf", mode = "wb")
## download.file("http://www.jstor.org/stable/pdfplus/25645718.pdf",
##               destfile = "jstor.pdf", mode = "wb")



setwd("C:/Users/Matthew/Documents/biblatex/RefManageR/doc/ReadPDFex2/bin")
bib <- ReadPDFs(".")
bib



pkg.names <- rownames(installed.packages())
pkg.bib <- lapply(pkg.names, function(pkg){
  refs <- as.BibEntry(citation(pkg))
  if (length(refs))
    names(refs) <- make.unique(rep(pkg, length(refs)))
  refs
})



pkg.bib <- RefManageR:::MakeCitationList(pkg.bib)
pkg.bib["boot"]
pkg.bib[key = "boot"]



BibOptions("check.entries")
BibEntry(bibtype = "Online", key = "ctan", date = "2006", 
 title = "The Comprehensive TeX Archive Network", url = "http://www.ctan.org")
old.opt.val <- BibOptions(check.entries = FALSE)
BibEntry(bibtype = "Online", key = "ctan", date = "2006", 
 title = "The Comprehensive TeX Archive Network", url = "http://www.ctan.org")
BibOptions(old.opt.val)  # restore the old value of the option



rjc.pm <- ReadPubMed("raymond j. carroll", database = "PubMed")
rjc.pm[[1L]]



ReadPubMed("journal of statistical software", field = "journal", retmax = 1, 
           mindate = 2009, maxdate = 2009)



GetPubMedRelated(rjc.pm, batch.mode = TRUE, max.results = 1)



oldhook <- knit_hooks$get("output")
newhook <- function(x, options) {
    return(paste0('\\begin{lstlisting}[showstringspaces=false,style=output,columns=fullflexible,breaklines=true,inputencoding=utf8,extendedchars=\true,breakautoindent=false,breakindent=0pt,inputencoding=utf8]\n',
      x,
    '\\end{lstlisting}'))
}
knit_hooks$set(output = newhook)



BibOptions(check.entries = FALSE)
ids <- rjc.pm$eprint[3:4]
ids
related <- GetPubMedRelated(ids, batch.mode = FALSE, max.results = c(1, 1),
                      return.sim.scores = TRUE, return.related.ids = TRUE)
toBiblatex(related)



knit_hooks$set(output = oldhook)



file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
bib <- ReadBib(file.name)
bib <- LookupPubMedID(bib, seq_len(10))
bib[eprinttype = "pubmed"][[1L]]  # print entry for first located ID



GetPubMedByID(unlist(bib$eprint)[1L])



knit_theme$set('fine_blue')
ReadZotero(user = '1648676', .params = list(q = 'bayesian', 
                               key = '7lhgvcwVq60CDi7E68FyE3br', limit = 2))



## RJC's Google Scholar profile is at: 
## http://scholar.google.com/citations?user=CJOHNoQAAAAJ
rjc.bib <- ReadGS(scholar.id = 'CJOHNoQAAAAJ', sort.by.date = TRUE, 
                  limit = 3)
rjc.bib



## RJC's Google Scholar profile is at: 
## http://scholar.google.com/citations?user=CJOHNoQAAAAJ
rjc.bib <- ReadGS(scholar.id = 'CJOHNoQAAAAJ', sort.by.date = FALSE, 
                  limit = 3)
rjc.bib
cbind(rjc.bib$cites, rjc.bib$bibtype)



## RJC's Google Scholar profile is at: 
## http://scholar.google.com/citations?user=CJOHNoQAAAAJ
rjc.bib <- ReadGS(scholar.id = 'CJOHNoQAAAAJ', sort.by.date = FALSE, 
                  limit = 10, check.entries = 'error')
rjc.bib2 <- ReadGS(scholar.id = 'CJOHNoQAAAAJ', sort.by.date = FALSE, 
                  limit = 10, check.entries = 'warn')
length(rjc.bib) == length(rjc.bib2)
## the offending entry.  RJC is missing because list of authors was too long
print(rjc.bib2[title='dietary measurement error'], 
      .opts = list(max.names = 99, bib.style = 'alphabetic'))



ReadCrossRef(query = 'rj carroll measurement error', limit = 3, 
             sort = "relevance", min.relevance = 80, verbose = FALSE)



file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- ReadBib(file.name, check = FALSE)
print(bib[author = "Nietzsche"], .opts = list(bib.style = "authoryear"))



old.opts <- BibOptions(bib.style = "alphabetic", max.names = 2, 
                       first.inits = FALSE)
bib[bibtype = "report"]
BibOptions(old.opts)  # reset to original values
print(bib[[19]], .opts = list(style = "html", no.print.fields = "url", 
      bib.style = "authortitle"))



knit_hooks$set(output = newhook)
ref <- BibEntry("thesis", key = "schieplthesis", date = "2011-03-17", url = 
"http://edoc.ub.uni-muenchen.de/13028/", urldate = "2014-03-06", title = 
"Bayesian Regularization and Model Choice for Structured Additive Regression", 
type = "phdthesis", institution = "LMU Munich", author = "Fabian Scheipl")
toBiblatex(ref)
toBibtex(ref, note.replace.field = "urldate")



knit_hooks$set(output = oldhook)



tmpfile <- tempfile(fileext = ".bib")
WriteBib(ref, file = tmpfile, biblatex = FALSE, verbose = FALSE)
library(bibtex)
read.bib(tmpfile)  
unlink(tmpfile)



## open(pkg.bib[["base"]])  # will use the 'url' field
## pkg.bib[["base"]]$file <- file.path(R.home("doc/manual"), "R-lang.pdf")
## open(pkg.bib[["base"]], open.field = c("file", "url"))



file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
bib <- ReadBib(file.name, check = FALSE)
# by default match.author = 'family.only' and ignore.case = TRUE
# inbook entry inheriting editor field from parent
bib[editor = "westfahl"]

# no match with parent entry, the returned child has inherited fields
bib[author = "westfahl"]



# Entries published in Zürich (in bib file Z{\"u}ich)
# OR entries written by Aristotle and published before 1930
bib[list(location="Zürich"),list(author = "Aristotle", year = '/1930')]

length(bib[author = "!knuth"])



file.name <- system.file("Bib", "RJC.bib", package="RefManageR")
bib <- ReadBib(file.name)
length(bib)
length(bib) == length(bib[author="Carroll"])
# which entries are missing RJC?
ind <- SearchBib(bib, author = "!Carroll", .opts = list(return.ind = TRUE))
bib[ind]$author  



bib <- bib[-ind[4L]]
bib[author="!Carroll"]$author <- c("Martinez, J. G. and Carroll, R. J.",
 "Carroll, R. J. and Ruppert, D. and Stefanski, L. A. and Crainiceanu, C. M.",
 "Carroll, R. J.", "Carroll, R. J.")
length(bib) == length(bib[author="Carroll"])



BibOptions(sorting = "none", bib.style = "alphabetic")
bib[seq_len(3)]
bib[seq_len(3)] <- list(c(date="2013-12"), ## add month to Serban et al.
        c(url="http://bsb.eurasipjournals.com/content/2013/1/13", 
          urldate = "2014-02-02"), ## add URL and urldate to Jennings et al.
        c(doi="10.1093/bioinformatics/btt608", 
          journal = "Bioinformatics")) ## add DOI and correct journal
bib[seq_len(3)]



BibOptions(sorting = "none", bib.style = "alphabetic")
bib2 <- bib[seq_len(3)]
bib2[2:3] <- bib[5:6]
# Note the Sarkar et al. entry is arXiv preprint with incorrect journal field
bib2
# Change type, remove journal, correct arXiv information
bib2[3] <- c(journal='', eprinttype = "arxiv", eprint = "1308.5427", 
           eprintclass = "math.ST", pubstate = "submitted", bibtype = "Misc")
bib2[3]



BibOptions(check.entries = FALSE, bib.style = "authoryear", style = "text")
file.name <- system.file("Bib", "biblatexExamples.bib", package = "RefManageR")
bib <- ReadBib(file.name)



PrintBibliography(bib, .opts = list(bib.style = "alphabetic"))


