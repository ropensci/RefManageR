# Mathew McLean
# Example code for RefManager.R
# October 22, 2013


library(RCurl)

curl = getCurlHandle()
z = getForm("http://scholar.google.com/scholar", q ='ruppert semiparametric regression', hl = 'en', btnG = 'Search',
            .opts = list(verbose = TRUE), curl = curl)

dd = htmlParse(z)
links = getNodeSet(dd, "//a[@href]")

# do something to identify the link you want
ind <- 35
found <- FALSE
while(!found){
  selected <- xmlGetAttr(links[[ind]], 'onclick')
  if(is.null(selected)){
    ind <- ind + 1
  }else{
    found <- TRUE
    m <- regexpr("\'.*?\'", selected)
    info <- regmatches(selected, m)
    info <- substring(info, 2, nchar(info)-1)
  }
}
bibURL <- paste('http://scholar.google.com/scholar.bib?q=info:', info, 
                ':scholar.google.com/&output=citation&hl=en&ct=citation&cd=0', sep='')
getURL(bibURL, curl=curl)

linkIWant <- links[[43]]
infoIWant <- "return gs_ocit(event,'cZRZo1sIwt4J','0')"
#grep('s/[^"]*"\([^"]*\)".*/\1/', linkIWant)
#grep('(?<=\').\\*?(?=\\')', linkIWant)
#grep('/"(?:[^`\\]|\\.)*`/', linkIWant)
#grep("(?:\\'|.)*?", linkIWant)
m <- regexpr("\'.*?\'", infoIWant)
info <- regmatches(linkIWant, m)
info <- substring(info, 2, nchar(info)-1)
shell.exec(paste('http://scholar.google.com/scholar.bib?q=info:', info, 
                   ':scholar.google.com/&output=citation&hl=en&ct=citation&cd=0', sep=''))

m <- regexpr("(?:\\'|.)*?", linkIWant)
regmatches(linkIWant, m)

tmp = getURL(linkIWant, curl = curl)

#############
# XML query to crossref

library(XML)
doiquery.xml <- xmlTreeParse('~/biblatex/doiquery.xml')

query <- doiquery.xml$doc$children$query_batch[["body"]]
#xmlValue(query[["author"]]) <- as.character(David Ruppert)
#addChildren(NULL, xmlValue(query[["author"]]) = as.character(David Ruppert))
add.query('David Ruppert', '2003', atitle='Semiparametric Regression', query=query)

citations <- read.csv("citations.csv")

new.query <- function(citation, query = query){
  xmlValue(query[["author"]]) <- as.character(citation$author)
  xmlValue(query[["year"]]) <- as.character(citation$year)
  xmlValue(query[["article_title"]][["text"]]) <- citation$title
  xmlValue(query[["journal_title"]]) <- citation$journal
  return(query)
}

add.query <- function(author, year, jtitle, atitle, query = query){
  if(!missing(author))
    xmlValue(query[["author"]]) <- author
  if(!missing(author))
    xmlValue(query[["year"]]) <- year
  if(!missing(author))
    xmlValue(query[["article_title"]]) <- atitle
  if(!missing(author))
    xmlValue(query[["journal_title"]]) <- jtitle
  return(query)
}


for (i in 1:nrow(citations)){
  q <- addChildren(q, add.query(citations[i,]))
}
axml <- addChildren(doiquery.xml$doc$children$query_batch, q )

saveXML(axml, file = 'foo.xml')


############################################################
# bibtex package

# useful stuff to learn from
# bibtex:::make.bib.entry
# bibtex:::read.bib
# bibtex:::make.citation.list
# bibtex:::write.bib
# utils:::bibentry
# tools:::BibTeX_entry_field_db  # non-bibtex compatible entries don't work here (applies to bibtex pkg)
# utils:::print.bibentry
# utils:::format.bibentry
# utils:::.bibentry_expand_crossrefs # non-bibtex compatible entries don't work (For printing/formatting)
# utils:::.bibentry_get_key
# utils:::.bibentry_check_bibentry1
bibtex:::make.bib.entry
bibtex:::make.citation.list
function (file = findBibFile(package), package = "bibtex", encoding = "unknown", 
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
    out <- lapply(out, make.bib.entry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- make.citation.list(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  out
}

tools:::BibTeX_entry_field_db
bibentry
utils:::print.bibentry

BibLaTeX_entry_field_db <- tools:::BibTeX_entry_field_db
BibLaTeX_entry_field_db$mvbook <- c('author', 'title', 'year|date')

# change fname to some document on your computer and path to the directory where it is located
# path can also be a url though if a value is specified for the url field in the bibtex entry,
#         it will be used instead
path <- "C:\\Users\\mmclean\\Documents\\Bayesian FGAM\\" # e.g. '~/papers/'
fname <- "Fabian-sim_sampler3c.pdf" #  e.g. 'McLean-ForecastingEMS(AoAS2011).pdf'
keywords <- 'EMS, time series, penalized splines, factor models, forecasting'
bibEntry <- "@article{matteson2011forecasting,
title={Forecasting emergency medical service call arrival rates},
author={Matteson, David S and McLean, Mathew W and Woodard, Dawn B and Henderson, Shane G},
journal={The Annals of Applied Statistics},
volume={5},
number={2B},
pages={1379--1406},
year={2011},
publisher={Institute of Mathematical Statistics}
}"

my.db <- add.refDB(refdf=NULL, bibEntry=bibEntry, path=path, filename=fname, keywords=keywords)
print(my.db, 1)
open(my.db, 1)

bibEntry <- "@article{mclean2012functional,
  title={Functional generalized additive models},
  author={McLean, Mathew W and Hooker, Giles and Staicu, Ana-Maria and Scheipl, Fabian and Ruppert, David},
  journal={Journal of Computational and Graphical Statistics},
  doi={10.1080/10618600.2012.729985},
  year={2012},
  publisher={Taylor \\& Francis Group}
}"

keywords <- 'FGAM, GAMs, functional data, semiparametric regression, penalized splines'
# don't need path if url or doi given in bibEntry
# path <- 'http://amstat.tandfonline.com/doi/full/10.1080/10618600.2012.729985#.UmU3kVCkrVY'

my.db <- add.refDB(my.db, bibEntry, keywords=keywords)
print(my.db,2)
open(my.db,2)
my.db
my.db <- remove.refDB(my.db, 1)
attr(my.db, 'size')

# take advantage of online entry type in biblatex for eprints
bibEntry <- "@online{mclean2013bayesian,
                    author = {McLean, M. W. and Scheipl, F. and Hooker, G. and Greven, S. and Ruppert, D.},
                    title = {Bayesian Functional Generalized Additive Models with Sparsely Observed Covariates},
                    urldate = {2013-10-06},
                    date = {2013},
                    eprinttype = {arxiv},
                    eprintclass = {stat.ME},
                    eprint = {1305.3585}
}"

keywords  <- "FGAM, missing data, functional data analysis, semiparametric regression"

my.db <- add.refDB(my.db, bibEntry, keywords=keywords)
open(my.db, 2)
my.db

search.refDB(my.db, searchterms='Ruppert', searchfields='author')
search.refDB(my.db, c('author', 'year'), c('mclean', '2011'))
search.refDB(my.db, c('author', 'year'), c('mclean','2012'))
search.refDB(my.db, c('keywords'), c('splines'))
getbibtex.refDB(my.db, 2)
