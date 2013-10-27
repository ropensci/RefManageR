biblatex
========

R package biblatex

To Do:
==================================================================================================================

* get DOIs from crossref
* convert all your RefManager.R functions to work with bibentry class
* getBibtex from google scholar function
* convert biblatex bib to bibtex for journal
* unicode stuff? --- Can bibtex package handle it?
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
* directed graph for plot function
        
DONE     
==================================================================================================================

* read from Zotero: http://www.zotero.org/support/dev/server_api/v2/read_requests
* open browser to Google Scholar for bibtex entry

==================================================================================================================