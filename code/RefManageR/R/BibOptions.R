# update '['
# implement bib.violation
# implement biblatex
# add convert function

########################################
# e.g.
# setRefClass('myRef', fields='myBool', methods=list(changeTemp = function(x, val, env=parent.frame()){
#  if(myBool){assign(deparse(substitute(x)), val, env)}}))
# test <- new('myRef', myBool=TRUE)
# temp <- 2
# test$changeTemp(temp, 3)
###############################

library(methods)
setRefClass('BibOptions', fields = c('match.author',  # criteria for matching author or editor.. 
                                                      # possible values: exact, family.name, given.initials
                                     'match.date',   # criteria for matching dates. p.v.: exact, year only
                                     'match.field',  # criteria for matching all other fields: 
                                     'return.ind',  # T/F should search return index of matches or bibentry objects
                                     'merge.fields.to.check',  # vector of fields for determining duplicate entries  `+` 
                                     'print.doi',      # should the DOI be printed?
                                     'bib.style',      # what bibstyle should be used? Possible biblatex
                                     'first.inits',   # should names in printing be abbreviated
                                     'dashed',         # should duplicate author names be replaced with \u2500
                                     'sorting',        # method to use for sorting (p. 44 of manual)
                                     'check.entries',          # should entries be checked for proper fields?
                                     'use.regex',      # are search terms regular expressions?
                                     'ignore.case',     # should case be ignored when searching?
                                     'max.names'       # maximum number of names to print
                                     )  #, methods = list(
#                                        duplicate.handler = function(...){
#                                          if(bib.violation == 'drop'){
#                                            # message about dropped entry
#                                          }else if (bib.violation == 'warn'){
#                                            # message about bad entry
#                                            # add entry anyway
#                                          }else{
#                                            
#                                          }
#                                        })  
            )
            

.BibOptions <- new('BibOptions', match.author='family.name', match.date='year.only', return.ind=FALSE, match.field='partial', 
                   merge.fields.to.check = 'key', print.doi = TRUE, bib.style = 'numeric', first.inits = TRUE, 
                   dashed = TRUE, sorting = NULL, check.entries = 'error', use.regex = TRUE, ignore.case = TRUE, max.names = 3
                   )
options(useFancyQuotes = FALSE)