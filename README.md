biblatex
========

R package biblatex

To Do:
==================================================================================================================
Important
-------------------
1. new open function; convert all other RefManager.R functions to work with bibentry class
2. read pdf's using pdftotext.exe; use list.files to add folder to database
* read bibliographies from pdftotext
3. "+.BibEntry" function for merging two bibentry classes: [S/O thread for overloading `+`](http://stackoverflow.com/questions/8022979/operator-overloading-and-class-definition-in-r-use-a-different-base-field-corpu). Also see ?Ops
  * handling duplicate entries
4. write to zotero: http://www.zotero.org/support/dev/server_api/v2/write_requests
5. boolean for how invalid entries are handled: either change to misc or drop (see what force=TRUE does in bibentry fnctn)
6. new `[` function e.g. db['author', 'mclean', 1:3] = first three entries in bib with mclean as author $\neq$ db[1:3, 'author', 'mclean']
7. add option for sorting results of subsetting based on diff fields and printing. See: utils:::sort.bibentry
8. plot function. sig.: plot(db, field, plottype, ...)  both 'author' and 'author-full' possible          
  * See igraph pkg or perhaps adapt http://bl.ocks.org/mbostock/3037015 (add counts on edges)
  * See the [wordcloud package](http://blog.fellstat.com/?cat=11)
9. make table function.  table(db, field)  both 'author' and 'author-full'    
10. change the print.bibentry function for when style='bibtex' and add style='biblatex'  
  * (i.e. convert biblatex to bibtex for submitting to journal.  
  * i.e. change toBibtex function and add toBibLaTeX function)
[S/O thread explaining how to do this](http://tex.stackexchange.com/questions/114787/converting-from-biblatex-to-bibtex-format-using-biber)  
have check=FALSE argument in print.BibEntry if user does not want to check proper format when printing biblatex or bibtex  
have printonly=FALSE argument in toBibtex.BibEntry and toBibLatex.BibEntry if user does not want converted BibEntry object returned
11. fix print to handle date field and 
12. implement a useful summary function
13. let users set "[.BibEntry" and "+.BibEntry" defaults using getOptions() or BibEntryOptions() accessors and mutators
14. Use everything here: http://ropensci.org/packages/
15. add ORCHID to person class
16. update print function to include index in database
17. vectorize fields in search function `all(pmatch(searchterm, fields))` perhaps?
18. Convert to and from data.frame for use with plyr

### Less Important/Smaller
* Make sure unicode handled properly? Unicode_alphabetic_tokenizer function in Unicode pkg  
Also see ?Encoding and ?iconv.  biblatex to bibtex function should use `iconv`
* http://opencitations.net/
* handle crossrefs for new BibLaTeX "in" fields
* write "-.BibEntry" function
* see the `scholar` R package.  Imports bibtex data using scholar id's
* see R package CITAN for "scientometrics"
* see knitcitations: install_github("knitcitations", "cboettig")
* handle Zotero using techreport for arXiv files
* add merge function as wrapper for "+.BibEntry"
* add search function as wrapper for "[.BibEntry"
* is [`stringi`](http://docs.rexamine.com/R-man/stringi/stringi-encoding.html) worth using over `Unicode` package?
* Distinguish what can be imported and needs to be re-implemented from `bibtex` and `utils`
* efficient ways to update database with only recently added papers, mention in JSS manuscript
* read all of R Journal paper on person and bibentry classes
* OR in search function - split
* discuss methods(class='bibentry') in JSS manuscript
* Review CrossRef [work on GitHub](https://github.com/gavinsimpson/orcid/blob/master/R/crossrefDOI.R)

DONE     
==================================================================================================================

* read from Zotero: http://www.zotero.org/support/dev/server_api/v2/read_requests
* open browser to Google Scholar for bibtex entry
* Biblatex entry types and fields supported
* Unicode can be supported with little effort. encoding option in WriteBib; Unicode pkg; Encoding function
* use unclass(bibentryObj) to get entry type and key
* make date field date object
* Extract operator works with date
* created names function to return keys of multiple entries or fields of one entry
* allow Extract function to take list argument
* allow Extract function to take vector of keys
* Exract works with entry types
* `$<-` works to assign fields from _bibentry_ class
* levels.BibEntry to access fields
* added unlist and relist functions. relist use somewhat odd.
* Can get DOIs or BibTeX entries from CrossRef

==================================================================================================================
