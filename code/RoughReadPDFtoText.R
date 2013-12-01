try(function(){print('hi')
    stop('err')
    2})

jinfo <- paste0(doc[source.ind:(publisher.ind-1)], collapse = ' ')

pattern <- paste0('^Source: ([[:print:]^,]+), Vol\\. ([0-9]+)(, No\\. ([0-9]+))? ',
  '\\(([[:upper:]][[:lower:]]{2}\\., )?([0-9]{4})\\), pp. ([[:digit:] -]+)')
m <- regexec(pattern, jinfo)
regmatches(jinfo, m)
pattern <- paste0('^Source: ([[:print:]^,]+), Vol\\. ([0-9]+)(, No\\. ([0-9]+)) ')
  '\\(([[:upper:]][[:lower:]]{2}\\., )?([0-9]{4})\\), pp. ')  # ([[:digit:]-])')
N <- 30

not.done <- seq.int(N)
res <- lapply(seq.int(N)[not.done], function(i){x <- sample(2, 1);if(x==2){NA}else{1}} )
not.done <- not.done[is.na(res)]

res <- lapply(seq.int(N)[not.done], function(i){x <- sample(2, 1);if(x==2){NA}else{1}} )
not.done <- not.done[is.na(res)]

length(not.done)

setwd('~/BayesFGAM/papers/')
path <- 'GelmanRubin(StatSci1992).pdf'
system2('pdftotext', paste0('-f 1 -l 1 ', path))
doc <- readLines(gsub('.pdf', '.txt', path))
system2('pdftotext', paste0('-f 2 -l 2 ', path))
doc2 <- readLines(gsub('.pdf', '.txt', path))

setwd('~/BayesFactorsOld/papers/')
path <- 'Chinese-PostPropInSSModel(Chinese2013).pdf'
path <- 'RouderMorey-BFforANOVA.pdf'
path <- 'Ray-XMCK_IP_detection.pdf'
temp <- system(paste0('pdfinfo ', path), intern = TRUE)
system2('pdftotext', paste0('-f 1 -l 2 ', path))
temp <- readLines(gsub('.pdf', '.txt', path))

pattern = "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'])\\S)+)\\b"
# pattern <- "(10[.][0-9]{4,}[^\\s\"/<>]*/[^\\s\"<>]+)"
# pattern <- '\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'<>])[[:graph:]])+)\b'
m <- regexpr(pattern, temp2, perl=TRUE)
regmatches(temp2, m)

m <- regexpr(pattern, temp, perl=TRUE)
doi <- regmatches(temp, m)
test2 <- ReadCrossRef(doi)
toBibtex(test2)



system2('pdftotext', '-l 2 -layout Scott-VarParmNear0slow.pdf M:/test.txt')
system2('pdftotext', '-l 2 Ohara-BayesVarSelectionReview.pdf')
system2('pdftotext', '-l 2 -layout ProprietyOfPosteriorsForGLMMs.pdf')
system2('pdftotext', '-l 1 -layout arxivtest.pdf')
system2('pdftotext', '-l 2 jasatest.pdf')
system2('pdftotext', '-l 2 mearxiv.pdf')
system2('pdftotext', '-l 1 -enc Latin1 pubmed1.pdf')
system2('pdftotext', '-l 1 pubmed2.pdf')
system2('pdftotext', '-l 1 Gelfand-ImproperPriorsForGLMs(JASA1999).pdf')
system2('pdftotext', '-f 2 -l 2 Gelfand-ImproperPriorsForGLMs(JASA1999).pdf')
system2('pdftotext', '-l 1 -layout jstorLT.pdf')
doc <- readLines('Scott-VarParmNear0slow.txt')
doc2 <- readLines('Ohara-BayesVarSelectionReview.txt')
doc3 <- readLines('ProprietyOfPosteriorsForGLMMs.txt')
doc4 <- readLines('arxivtest.txt')
jasa <- readLines('jasatest.txt')
docme <- readLines('mearxiv.txt', encoding = 'UTF-8')
docpm1 <- readLines('pubmed1.txt', encoding = 'UTF-8')
docpm2 <- readLines('pubmed2.txt', encoding = 'UTF-8')
doc1 <- readLines('Gelfand-ImproperPriorsForGLMs(JASA1999).txt', encoding = 'UTF-8')
doc2 <- readLines('Gelfand-ImproperPriorsForGLMs(JASA1999).txt', encoding = 'UTF-8')
jstordoc2 <- readLines('jstorLT.txt', encoding = 'UTF-8')

setwd('~/BayesFGAM/papers')
system2('pdftotext', '-l 2 Marriott-DiagnosticsForVB(arXiv2013).pdf')
system2('pdftotext', '-f 2 -l 2 CowlesCarlin-MCMCdiagnosticsReview(JASA1996).pdf')
system2('pdftotext', '-f 1 -l 1 -layout MengersenTweedie-ConvRatesForMH(AoS1996).pdf')
doc2 <- readLines('MengersenTweedie-ConvRatesForMH(AoS1996).txt', encoding = 'UTF-8')
doc <- readLines('CowlesCarlin-MCMCdiagnosticsReview(JASA1996).txt')
docarx <- readLines('Marriott-DiagnosticsForVB(arXiv2013).txt')
tnames <- c('Ana-Maria Staicu', 'A.-M. Staicu', 'R. de Vries', 'Oscar de la Hoya', 'Mathew McLean', 
            'Manuel Febrero-Bande', 'Shaq O\'Neal', 'T. T. T. Smith')
# author     first name                 opt. middle name          last name
#ind <- grep("[A-Z][a-z]*[\\.]?[[:space:]-][A-Z]?[a-z]*[\\.]?[[:space:]]?[[:upper:]][[:alpha:]'-]+", docme)

# match one author
ind <-  grep("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", docme)
m <- regexpr("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", doc2)
regmatches(doc2, m)
ind <- grep("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", tnames)
m <- regexpr("[A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+", tnames)
regmatches(tnames, m)

# match multiple authors
mnames <- c('M. W. McLean, Ana-Maria Staicu, David Ruppert', 'Giles Hooker', 'Manny Pacquiao and T. L. Lewis')
ind <- grep("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+([[:print:]]*)?)+", mnames)
ind2 <- grep("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+[[:space:]]?(and)?[,;&]?[[:space:]])+", mnames)

m <- regexpr(paste0("([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
"[[:upper:]][[:alpha:]'-]+[[:space:]]?",  # last name potential extra char to 
"(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                    mnames)
m <- regexpr("([A-Z][a-z]*[\\.]?[[:space:]-]([A-Z]?[a-z]*[\\.]?[[:space:]-])*[[:upper:]][[:alpha:]'-]+([[:print:]]*)?)+", mnames)
regmatches(mnames, m)

gsub("[^[:alpha:] '-]", '', tempnames)  # remove special characters end of names (e.g. McLean*, Carroll^#)

# e.g.: Ana-Maria Staicu, A.-M. Staicu, R. de Vries, Oscar de la Hoya, Mathew McLean, Manuel Febrero-Bande
# Shaq O'Neal, T. T. T. Smith
# ind <- regexpr('[A-Z]\\.|[a-z]+[.]?[[:space:]][A-Z]?[a-z]*[.]?[[:space:]]?[A-Z][a-z]+', docme)
# ind <- regexpr('[[:upper:]](\\.|[a-z]+)[.]?[[:space:]][A-Z]?[a-z]*[.]?[[:space:]]?[[:upper:]][[:alpha:]]+', docme)
# regmatches(docme, ind)




aut.ind <- regexpr(paste0(# invalid words negate match
  "(?!Online|Supplement|Data|University|College|Institute|School)",
  "^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
                          "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
                          "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
                          "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
  "(?<!Online|Supplement|Data|University|College|Institute|School)", 
                          "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
                   doc, perl=TRUE)
regmatches(doc, aut.ind)


# if(is.na(year)){
#   year <- NULL
# }else{
#   year <- 
#     sub('\\<(19|20)([0-9]{2})\\>', '', year)
#   grep('\\<(19|20)([0-9]{2})\\>', doc3, value=TRUE)[1]
# }

# URL

# title

# journal

GetAuthorTitleWLayout <- function(doc, found.abstract){
 # browser()
  abst.ind <- grep('^[[:space:]]*A[Bb][Ss][Tt][Rr][Aa][Cc][Tt]|^S[Uu][Mm][Mm][Aa][Rr][Yy]|^S[Yy][Nn][Oo][Pp][Ss][Ii][Ss][:.]?\\>', doc)
  if (length(abst.ind) && abst.ind > 2L){  # assume title/author comes before Abstract. need 2nd cond. for ind==1
    doc <- doc[1L:(abst.ind - 1L)]
    found.abstract <- TRUE
  }
  browser()

   N <- length(doc)
  i <- 1
  first.match <- FALSE
  done.match <- FALSE
  while (i <= N && !done.match){
    title.ind <- regexpr(paste0("(?!", BAD.WORDS, ")", "^[[:space:]]+",
                                "[[:upper:]][[:alpha:]'-]*(,|-|:)?[ -]",
                                #"([[:alpha:]:,' ]){2,}(\\.|!|\\?)?$"),
                                "([[:alpha:]:,' -]+){2,}[[:print:]]?$",
                                "(?<!", BAD.WORDS, ")"),
                         doc[i], perl = TRUE)
    if (title.ind != -1){
      if (!first.match){
        first.match <- TRUE
        title.match <- regmatches(doc[i], title.ind)
      }else{
        title.match <- c(title.match, regmatches(doc[i], title.ind))  
      }
    }else if (first.match){
      done.match <- TRUE
    }
    i <- i + 1
  }
  
  #  browser()
  #   aut.ind <- regexpr(paste0("^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  #                       "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
  #                       "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
  #                       "(, )?(Jr)?(II)?(III)?(IV)?(, )?(MD)?(, )?(Ph|HD)?.?",  # optional qualifications      
  #                       "(and)?[,;&$]?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
  #                doc)
  
  #   aut.ind <- regexpr(paste0(# invalid words negate match
  #     "(?!Online|Supplement|Data|University|College|Institute|School)",
  #     "^([A-Z][a-z]*[\\.]?[ -]",  # first name, maybe hypenated or abbrev.
  #     "([A-Z][a-z]*[\\.]?[ -])*",  # optional middle name or initial, maybe hypenated
  #     "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",  # last name + potential extra char to 
  #     "(, Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications      
  #     "(?<!Online|Supplement|Data|University|College|Institute|School)", 
  #     "(and)?([,;&$].?)?[[:space:]]?)+$"),  # and, ",", ";", or "&" to seperate names. Repeat
  #                      doc, perl=TRUE)
  BAD.WORDS <- paste0('Online|Supplement|Data|University|College|Centre|Center|Working|Faculty|Science',
                      '|\\b[Oo][Ff]\\b|\\b[Tt][Hh][Ee]\\b|Foundation|Series|Paper|\\b[Uu][Rr][Ll]\\b|Research|Labs',
                      '|Institute|School|Technical|Department|Staff')
  aut.ind <- regexpr(paste0(# invalid words negate match
    "^[[:space:]]+", "(?!", BAD.WORDS,  ")", 
    "(B[Yy] | A[Uu][Tt][Hh][Oo][Rr][Ss]?:?)?",
    "([[:upper:]][[:alpha:]]*[\\.]?[ -]",            # first name, maybe hypenated or abbrev.
    "([[:upper:]][[:alpha:]]*[\\.]?[ -])*",           # optional middle name or initial, maybe hypenated
    "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",        # last name + potential extra char to 
    "(, Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications      
    "(?<!", BAD.WORDS, ")", 
    "(,.|;.)*([[:space:]]*[Aa][Nn][Dd]| &)?[[:space:]]*)+$"),             # and, ",", ";", or "&" to seperate names. Repeat
                     doc[-1], perl=TRUE)              # first line can't have authors
  aut.match <- regmatches(doc[-1], aut.ind)
  if (length(aut.match) == 0){
    aut.match <- NULL
  }else{
    aut.match <- gsub("(,? MD)?(,? P(H|h)D)?", '', aut.match)  # remove MD and PhD
    aut.match <- gsub("[^[:alpha:] ,'-]", '', aut.match)  # remove punct at end of last name
    aut.match <- gsub("^[[:space:]]*Author( |: )|^[[:space:]]*By( |: )", '', aut.match)  # remove author or by at start
  }
  
  match.ind <- which(aut.ind > -1L) + 1  # +1 because had doc[-1] above
  # if didn't find abstract, make attempt at not including names from doc body
  if (!found.abstract && length(aut.match) > 1L){
    spaces <- diff(match.ind)
    first.too.big <- which(match.ind > 2L)[1]
    if (!is.na(first.too.big))
      aut.match <- aut.match[1L:first.too.big]
  }
  BAD.WORDS <- paste0('\\bSupplement\\b|University|\\bCollege\\b|\\bCentre\\b|\\bCenter\\b|Working|Faculty',
                      '|Paper|\\b[Uu][Rr][Ll]\\b|Labs|\\bJournal\\b|Institute|\\bSchool\\b')
  if (length(match.ind)){  # if found author, assume title comes before author
    ind <- match.ind[1]
    doc <- doc[(ind-1L):1L]  # reverse doc, assume title comes just before authors  
  }
  
  # starting either author match and going backwards, or starting from line 1, search for title
  # have two flags to allow for multiline titles
 
      
#   }else{
#     title.ind <- regexpr(paste0("(?!", BAD.WORDS, ")",
#                                 "^[[:upper:]][[:alpha:]'-]*(,|-|:)?[ -]",
#                                 "([[:alpha:]:,' -]){2,}(\\.|!|\\?)?$",
#                                 "(?<!", BAD.WORDS, ")"),
#                          doc, perl = TRUE)
#   }
  # title.match <- regmatches()
  if (!first.match){
    title.match <- NULL
  }else{
    if (length(match.ind)){  # undo reversing of doc when author matched
      if (i - 3 > 0){
        if (length(grep("[[:alpha:]', -]+", doc[i-3])))
          title.match <- c(title.match, doc[i-3])
      }
      title.match <- rev(title.match)
    }
    title.match <- paste0(title.match, collapse = ' ')
    # simple fix for case troubles
    title.match <- gsub("([[:alpha:]])([[:alpha:]'-]*)", "\\U\\1\\L\\2", title.match, perl=TRUE)
    title.match <- gsub('[^\\w]$', '', title.match)  # remove superscripted char indicating footnote for title
  }

#   }else{
#     match.ind <- which(title.ind > -1L)
#     if (length(match.ind) != 1L){
#       spaces <- diff(match.ind)
#       first.too.big <- which(spaces > 2L)[1]
#       if (!is.na(first.too.big))
#         title.match <- title.match[1L:first.too.big]
#     }
#   }
  
  #return(list(ind=which(aut.ind != -1L), match=aut.match, ab.ind=ind, title.match))
  return(list(author = aut.match, title = paste0(title.match, collapse = ' '), 
              found.abstract = found.abstract))
}

test <- c('Mathew W. McLean', "Science", "Research Institute", "Happy Institute", 
          "Sean O'Rielly", 'Jane-Lin Wang', 'Cornell University', 'T.M. Lewin', 'Oscar de la Hoya',
          "Oscar was a loser")
aut.ind <- regexpr(paste0(# invalid words negate match
  #  "(?!", BAD.WORDS,  ")",
 # "^((?!", BAD.WORDS, ").)*$",
    "^(B[Yy] ?|A[Uu][Tt][Hh][Oo][Rr][Ss]?:? ?)?",
    "([[:upper:]][[:alpha:]]*[\\.]?[ -]",            # first name, maybe hypenated or abbrev.
    #"((v[oa]n( (de|den|der))?|de la|de|del|) )?",     # allows some extra Dutch and Spanish names
    "([[:alpha:]]*[\\.]?[ -])*",          # optional middle name or initial, maybe hypenated
    # "([[:upper:]][[:alpha:]]*[\\.]?[ -])*",          # optional middle name or initial, maybe hypenated
    "[[:upper:]][[:alpha:]'-]+.?[[:space:]]?",        # last name + potential extra char to 
    "(, Jr| II| III| IV)?(,? MD.?)?(,? P(h|H)D.?)?",  # optional qualifications      
  #  "(?<!", BAD.WORDS, ")", 
    "(,.|;.)*( and| &)?[[:space:]]?)+$"),             # and, ",", ";", or "&" to seperate names. Repeat
                     docpm1, perl=FALSE)  
aut.match <- regmatches(docpm1, aut.ind); aut.match

regmatches(aut.match, regexpr(paste0("^((?!", BAD.WORDS, ").)*$"), aut.match, perl=TRUE))
regmatches(aut.match, regexpr(paste0("^.*(?!", BAD.WORDS, ").*$"), aut.match, perl=TRUE))