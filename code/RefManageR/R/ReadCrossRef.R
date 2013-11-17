# Mathew McLean
# 2013-11-07
# tools for interacting with Crossref
# TO DO: lookupDOI add DOI to bibentry
#        search crossref for bibtex
#        open bibentry using DOI
#        get bibtex using DOI
# Note: false positives likely, false negatives unlikely
# http://search.crossref.org/help/api
# http://labs.crossref.org/resolving-citations-we-dont-need-no-stinkin-parser/

require(RCurl)
require(RJSONIO)

# http://www.crossref.org/openurl?pid=name@someplace.com&aulast=Maas%20LRM&title= JOURNAL%20OF%20PHYSICAL%20OCEANOGRAPHY&volume=32&issue=3&spage=870&date=2002
# 
# # works!!
# temp <- getURLContent(url='http://dx.doi.org/10.1126/science.169.3946.635',
#                       .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"), verbose=TRUE, followLocation=TRUE))
# 
# results <- getForm("http://search.labs.crossref.org/dois", q="carroll ruppert")
# fromj <- RJSONIO::fromJSON(results)
# 
# SearchCrossRef('Ruppert Carrol statistics')
# SearchCrossRef('Carberry, J 2008, “Toward a Unified Theory of High-Energy Metaphysics: Silly String Theory.” Journal of Psychoceramics, vol. 5, no. 11, pp. 1-3.')

ReadCrossRef <- function(query, limit = 5, sort = 'relevance', year = NULL, min.relevance = 80,
                           temp.file = tempfile(fileext = '.bib'), delete.file = TRUE, verbose = FALSE){
  results <- getForm("http://search.labs.crossref.org/dois", q=query, year=year, sort=sort,  
                     rows=limit)
  
  if (delete.file)
    on.exit(unlink(temp.file, force = TRUE))
  
  fromj <- RJSONIO::fromJSON(results)
  num.res <- min(limit, length(fromj))

  if (num.res > 0){
    file.create(temp.file)
   # entries <- vector('character', num.res)
    relevancies <- numeric(num.res)
    for(i in 1:num.res){
      if(verbose){
        message(paste0('Including Entry: ', fromj[[i]]$fullCitation))
        message(paste0('Relevancy score: ', fromj[[i]]$normalizedScore))
      }
      if(fromj[[i]]$normalizedScore >= min.relevance){
        temp <- getURLContent(url=fromj[[i]]$doi,
                            .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"), followLocation=TRUE))
        if(is.raw(temp))
          temp <- rawToChar(temp)
        write(temp, file = temp.file, append=TRUE)
      }
    }
  }
 # write(temp, file = temp.file, append=TRUE)
  bib.res <- ReadBib(file=temp.file, encoding='UTF-8')

  return(bib.res)
}

# LookupDOI
# 
# GetBibEntryWithDOI('10.1198/106186002853')



# also works
# searchCrossRef <- function(email, author, title, date, multihit=FALSE){
#   form <- getForm('http://www.crossref.org/openurl', pid=email, aulast=author, title=title,
#                   date=date, multihit=multihit, .opts=list(verbose=TRUE))
#   browser()
# }
# 
# searchCrossRef('mathew.w.mclean@gmail.com', author='ruppert', title='semiparametric regression', date='2002',
#                multihit=TRUE)
# 
# searchCrossRef('mathew.w.mclean@gmail.com', author='carroll', 
#                title='generalized partially linear single-index models', date='1997')
# searchCrossRef('mmclean@stat.tamu.edu', author='carroll', 
#                title='journal of the american statistical association', date='1997')
# 
# getURL('http://www.crossref.org/openurl?pid=name@someplace.com&aulast=Maas%20LRM&title=&date=1997')
# 
# "Accept: application/vnd.citationstyles.csl+json, application/rdf+xml" http://dx.doi.org/10.1126/science.169.3946.635
# 
# getURLContent(url='http://dx.doi.org/10.5555/12345678?pid=mmclean@stat.tamu.edu', header=TRUE,
#               httpheader = c(Accept = "application/x-bibtex"), verbose=TRUE)
# 
# h <- basicTextGatherer()
# curlPerform(url = "http://dx.doi.org/10.1080/01621459.2013.838568", writefunction = h$update, 
#             httpheader = c(Accept = "application/vnd.crossref.unixref+xml;q=1, application/rdf+xml;q=0.5"))
# h$value()
# #curlPerform(getCurlHandle(url='http://dx.doi.org/10.1080/01621459.2013.838568',
# #              .opts = list(httpheader = c(Accept = "application/x-bibtex"), verbose=TRUE)))
# temp <- getURLContent(url='http://dx.doi.org/10.1126/science.169.3946.635',
#        .opts = curlOptions(httpheader = c(Accept = "application/x-bibtex"), verbose=TRUE, followLocation=TRUE))
# read.delim(textConnection(temp))
# getURLContent(url='http://dx.doi.org/10.1126/science.169.3946.635',
#               .opts=list(httpheader = c('Accept' = "application/x-bibtex", 'Content-Type' = "application/x-bibtex"), verbose=TRUE))
# getURLContent(url='http://dx.doi.org/10.1126/science.169.3946.635',
#               httpheader=c('Accept' ='text/bibliography; style=bibtex'), verbose=TRUE)
# getURLContent(url='http://dx.doi.org/10.1126/science.169.3946.635')
