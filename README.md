biblatex
========

R package biblatex

To Do:
==================================================================================================================
Big
-------------------
* get DOIs from crossref
* convert all your RefManager.R functions to work with bibentry class
* unicode stuff? --- Can bibtex package handle it?: Unicode_alphabetic_tokenizer function in Unicode pkg  
  Also see ?Encoding and ?iconv.  biblatex to bibtex function should use `iconv`
* see how far changing BibTeX_entry_field definition gets you with bibtex package and bibentry function
* use list.files to add folder to database
* read pdf's using pdftotext.exe
* read bibliographies from pdftotext
* write to zotero: http://www.zotero.org/support/dev/server_api/v2/write_requests
* see R package CITAN for "scientometrics"
* see knitcitations: install_github("knitcitations", "cboettig")         
* merging two bibentry classes (probably already done): [S/O thread for overloading `+`](http://stackoverflow.com/questions/8022979/operator-overloading-and-class-definition-in-r-use-a-different-base-field-corpu). Also see ?Ops
* handling duplicate entries
* boolean for how invalid entries are handled: either change to misc or drop
* handle Zotero using techreport for arXiv files
* new open function
* new `[` function e.g. db['author', 'mclean', 1:3] = first three entries in bib with mclean as author $\neq$ db[1:3, 'author', 'mclean']
* add option for sorting results of subsetting based on diff fields and printing. See: utils:::sort.bibentry
* search function that repeats `[`
* directed graph for plot function. See igraph pkg or perhaps adapt http://bl.ocks.org/mbostock/3037015 (add counts on edges)
* See the [wordcloud package](http://blog.fellstat.com/?cat=11)
* Distinguish what can be imported and needs to be re-implemented from `bibtex` and `utils`
* is [`stringi`](http://docs.rexamine.com/R-man/stringi/stringi-encoding.html) worth using over `Unicode` package?
* make table fnctn generic  table(db, field)  both 'author' and 'author-full'    
* plot(db, field, plottype, ...)  both 'author' and 'author-full' possible          
* change the print.bibentry function for when style='bibtex' and add style='biblatex'  
(i.e. convert biblatex to bibtex for submitting to journal.  
i.e. change toBibtex function and add toBibLaTeX function)
[S/O thread explaining how to do this](http://tex.stackexchange.com/questions/114787/converting-from-biblatex-to-bibtex-format-using-biber)  
have check=FALSE argument in print.BibEntry if user does not want to check proper format when printing biblatex or bibtex  
have printonly=FALSE argument in toBibtex.BibEntry and toBibLatex.BibEntry if user does not want converted BibEntry object returned

* fix print to handle date field and 
* implement a useful summary function
### Less Important
* http://opencitations.net/
* handle crossrefs for new BibLaTeX "in" fields
* write "-.BibEntry" function
DONE     
==================================================================================================================

* read from Zotero: http://www.zotero.org/support/dev/server_api/v2/read_requests
* open browser to Google Scholar for bibtex entry

==================================================================================================================