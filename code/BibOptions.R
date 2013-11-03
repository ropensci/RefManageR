# update '['
# implement bib.violation
# implement biblatex
# add convert function

########################################
# e.g.
# setRefClass('myRef', fields='myBool', methods=list(changeTemp = function(x, val, env=parent.frame()){
#  if(myBool){assign(deparse(substitute(x)), val, env)}}))
# poop <- new('myRef', myBool=TRUE)
# temp <- 2
# poop$changeTemp(temp, 3)
###############################

library(methods)
setRefClass('BibOptions', fields = c('match.author',  # criteria for matching author or editor.. 
                                                      # possible values: exact, family.name, given.initials
                                     'match.date',   # criteria for matching dates. p.v.: exact, year only
                                     'match.field',  # criteria for matching all other fields: 
                                     'return.ind',  # T/F should search return index of matches or bibentry objects
                                     'duplicate.check',  # how should duplicate entries be determined in `+` 
                                                         # pv.: 'key', 'key.title', 'key.title.author', 'key.bibtype'
                                     'bib.violation'   # how to handle violations when creating bibentries? 
                                                       # p.v.: 'drop', 'warn', 'to.misc'
                                     ), methods = list(
                                       duplicate.handler = function(...){
                                         if(bib.violation == 'drop'){
                                           # message about dropped entry
                                         }else if (bib.violation == 'warn'){
                                           # message about bad entry
                                           # add entry anyway
                                         }else{
                                           
                                         }
                                       })  
            )
            

.BibOptions <- new('BibOptions', match.author='family.name', match.date='year.only', return.ind=FALSE, match.field='partial', 
                   duplicate.check = 'key.only', biblatex=TRUE, bib.violation = 'error')
