BibLaTeX_entry_field_db <- list()  # tools:::BibTeX_entry_field_db

BibLaTeX_entry_field_db$Article <- c('author', 'title', 'journaltitle|journal', 'year|date')

BibLaTeX_entry_field_db$Book <- c('author|editor|translator', 'title', 'year|date')

# multivolume book
BibLaTeX_entry_field_db$MVBook <- c('author|editor|translator', 'title', 'year|date')  

# different from BibTeX
BibLaTeX_entry_field_db$InBook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# e.g. book reprint in selected works of author
BibLaTeX_entry_field_db$BookInBook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# supplemental material for book
BibLaTeX_entry_field_db$SuppBook <- c('author', 'title', 'booktitle|maintitle', 'year|date')  

# book without formal publisher
BibLaTeX_entry_field_db$Booklet <- c('author|editor', 'title', 'year|date')  

# single-volume work, with lots of separate individual works
BibLaTeX_entry_field_db$Collection <- c('editor', 'title', 'year|date') 

# multi-volume collection
BibLaTeX_entry_field_db$MVCollection <- c('editor', 'title', 'year|date') 

# contribution to a collection
BibLaTeX_entry_field_db$InCollection <- c('author', 'editor', 'title', 'booktitle', 'year|date') 

# supplement to a collection
BibLaTeX_entry_field_db$SuppCollection <- c('author', 'editor', 'title', 'booktitle', 'year|date') 

#manual
BibLaTeX_entry_field_db$Manual <- c('author|editor', 'title', 'year|date') 

# misc
BibLaTeX_entry_field_db$Misc <- character(0)

# online
BibLaTeX_entry_field_db$Online <- c('author|editor', 'title', 'year|date', 'url|eprinttype') 

# patent
BibLaTeX_entry_field_db$Patent <- c('author', 'title', 'number', 'year|date') 

# periodical
BibLaTeX_entry_field_db$Periodical <- c('editor', 'title', 'year|date') 

# supplement to periodical; alias for article in most styles; e.g. regular columns, obituaries, letters to editor, etc.
BibLaTeX_entry_field_db$SuppPeriodical <- c('editor', 'title', 'year|date') 

# proceedings
BibLaTeX_entry_field_db$Proceedings <- c('editor', 'title', 'year|date') 

# multi-volume proceedings
BibLaTeX_entry_field_db$MVProceedings <- c('editor', 'title', 'year|date') 

# inproceedings: an article in conference proceedings
BibLaTeX_entry_field_db$InProceedings <- BibLaTeX_entry_field_db$Proceedings

# reference: alias for collection for most styles; specific type of collection e.g. encyclopedia or dictionary
BibLaTeX_entry_field_db$Reference <- c('editor', 'title', 'year|date') 

# multi-volume reference: alias for mvcollection for most styles; 
BibLaTeX_entry_field_db$MVReference <- BibLaTeX_entry_field_db$Reference

# inreference: alias for incollection for most styles; 
BibLaTeX_entry_field_db$InReference <- BibLaTeX_entry_field_db$Reference

# report: need to specify type field. replaces techreport
BibLaTeX_entry_field_db$Report <- c('author', 'title', 'type', 'institution', 'year|date') 

# set: special, see Section 3.11.5 of biblatex manual
BibLaTeX_entry_field_db$Set <- c('entryset')

# thesis: replaces 'mastersthesis' and 'phdthesis' types by adding 'type' field
BibLaTeX_entry_field_db$Thesis <- c('author', 'title', 'type', 'institution', 'year|date')  

# unpublished
BibLaTeX_entry_field_db$Unpublished <- c('author', 'title', 'year|date') 

# xdata. special containers for data to be enherited by other entries which specify an 'xdata' field; see Section 3.11.6 of bibtex manual
BibLaTeX_entry_field_db$XData <- character(0)

######################
# custom types # not supported by standard bib. styles, for which they will be treated as 'misc' type
BibLaTeX_entry_field_db$`Custom[a]` <- character(0)
BibLaTeX_entry_field_db$`Custom[b]` <- character(0)
BibLaTeX_entry_field_db$`Custom[c]` <- character(0)
BibLaTeX_entry_field_db$`Custom[d]` <- character(0)
BibLaTeX_entry_field_db$`Custom[e]` <- character(0)
BibLaTeX_entry_field_db$`Custom[f]` <- character(0)

#######################################
# aliases for compatibility with BibTeX
BibLaTeX_entry_field_db$Conference <- BibLaTeX_entry_field_db$InProceedings
BibLaTeX_entry_field_db$Electronic <- BibLaTeX_entry_field_db$Online
BibLaTeX_entry_field_db$MastersThesis <- c('author', 'title', 'school', 'year')
BibLaTeX_entry_field_db$PhdThesis <- c('author', 'title', 'school', 'year')
BibLaTeX_entry_field_db$TechReport <- c('author', 'title', 'institution', 'year')
BibLaTeX_entry_field_db$Www <- BibLaTeX_entry_field_db$Online

##########################################
# unsupported types: converted to 'misc' type by standard bib styles that don't support them
# no checking for required fields
BibLaTeX_entry_field_db$Artwork <- character(0)
BibLaTeX_entry_field_db$Audio <- character(0)
BibLaTeX_entry_field_db$BibNote <- character(0)
BibLaTeX_entry_field_db$Commentary <- character(0)
BibLaTeX_entry_field_db$Image <- character(0)
BibLaTeX_entry_field_db$Jurisdiction <- character(0)
BibLaTeX_entry_field_db$Legislation <- character(0)
BibLaTeX_entry_field_db$Legal <- character(0)
BibLaTeX_entry_field_db$Letter <- character(0)
BibLaTeX_entry_field_db$Movie <- character(0)
BibLaTeX_entry_field_db$Music <- character(0)
BibLaTeX_entry_field_db$Performance <- character(0)
BibLaTeX_entry_field_db$Review <- character(0)
BibLaTeX_entry_field_db$Software <- character(0)
BibLaTeX_entry_field_db$Standard <- character(0)
BibLaTeX_entry_field_db$Video <- character(0)


# BibTeX
BibTeX_entry_field_db <- list(
Article = c("author", "title", "journal", "year"),   
Book = c("author|editor", "title", "publisher", "year"),         
Booklet = "title",
InBook = c("author|editor", "title", "chapter", "publisher",  "year"),         
InCollection = c("author", "title", "booktitle", "publisher", "year"),     
InProceedings = c("author", "title", "booktitle", "year"),     
Manual = "title",
MastersThesis = c("author", "title", "school", "year"),  
Misc = character(0),
PhdThesis = c("author", "title",  "school", "year"),  
Proceedings =  c("title", "year"), 
TechReport = c("author", "title", "institution", "year"),       
Unpublished = c("author", "title",  "note"))
