#' @keywords internal
AddCite <- function(index, use.hyper = TRUE){
  new.ind <- logical(length(index))
             if (use.hyper)
               new.ind <- !new.ind
  names(new.ind) <- index
  # .cites$indices <- unique(c(.cites$indices, index))
  tmp <- c(.cites$indices, new.ind)
  .cites$indices <- tmp[!duplicated(names(tmp))]
}

#' Cite a BibEntry object in text and print all citations
#' 
#' The \code{Cite} functions allow for citing a \code{BibEntry} object in text.  The 
#' \code{PrintBibliography} function allows for printing the bibliography of all 
#' the cited entries.  The \code{NoCite} function adds references to the bibliography
#' without including a citation.  These functions are most useful when used in, 
#' e.g., a RMarkdown or RHTML document.
#' 
#' @param bib a BibEntry object
#' @param ... passed to \code{\link{SearchBib}} for indexing into bib.  A character vector of 
#' keys, for example.
#' @param textual logical; if TRUE, a \dQuote{textual} citation is produced, i.e.
#' what is produced by \\citet in \code{natbib} and \\textcite in \code{BibLaTeX}; otherwise,
#' a parenthetical citation as \\citep and \\autocite.
#' @param before string; optional text to display before the citation.
#' @param after string; optional text to display after the citation.
#' @param .opts list; See the relevant section in \code{\link{BibOptions}} for a description
#' of all valid options for these functions.
#' @details   Read and execute the files in the examples.
#' @return For the cite functions: a character string containing the citation
#' @rdname Cite
#' @export
#' @aliases PrintBibliography
#' @examples 
#' file <- system.file("Bib", "biblatexExamples.bib", package = "RefManageR")
#' BibOptions(check.entries = FALSE)
#' bib <- ReadBib(file)
#' Citet(bib, 12)
#' Citep(bib, "loh", .opts = list(cite.style = "numeric"), before = "see ")
#' Citet(bib, "loh", .opts = list(cite.style = "numeric", super = TRUE))
#' AutoCite(bib, eprinttype = "arxiv", .opts = list(cite.style = "authoryear"))
#' Citep(bib, author = "kant")
#' # shorthand field in both entries gets used for numeric and alphabetic labels
#' TextCite(bib, author = "kant", .opts = list(cite.style = "alphabetic"))
#' TextCite(bib, author = "kant", .opts = list(cite.style = "numeric"))
#' TextCite(bib, author = "kant", .opts = list(cite.style = "alphabetic", style = "html"))
#' punct <- unlist(BibOptions("bibpunct"))
#' punct[3:4] <- c("(", ")")
#' TextCite(bib, 33, .opts = list(bibpunct = punct, cite.style = "alphabetic"))
#' 
#' BibOptions(restore.defaults = TRUE)
#' \dontrun{
#' library(knitr)
#' ## See also TestNumeric.Rmd and TestAlphabetic.Rmd for more examples
#' doc <- system.file("Rmd", "rmdExample.Rmd", package = "RefManageR")
#' file.show(doc)
#' tmpfile <- tempfile(fileext = ".html")
#' knit(doc, tmpfile)
#' browseURL(tmpfile)
#' unlink(tmpfile)
#' doc <- system.file("Rhtml", "TestAuthorYear.Rhtml", package = "RefManageR")
#' file.show(doc)
#' tmpfile <- tempfile(fileext = ".html")
#' knit(doc, tmpfile)
#' browseURL(tmpfile)
#' unlink(tmpfile)
#' }
Cite <- function(bib, ..., textual = FALSE, before = NULL, after = NULL, 
                 .opts = list()){
    if (length(.opts)){
      old.opts <- BibOptions(.opts)
      on.exit(BibOptions(old.opts)) 
    }

    result <- with(BibOptions(), {
      style <- .BibEntry_match_format_style(style)
      papers <- suppressMessages(do.call(`[.BibEntry`, list(x = bib, ...)))
      if (!length(papers))
        return("")
      
      shortName <- function(person) {
          if (length(person$family)) 
              paste(cleanupLatex(person$family), collapse = " ")
          else paste(cleanupLatex(person$given), collapse = " ")
      }
      authorList <- function(paper){
        names <- sapply(paper$author, shortName)
        if (!length(names))
          names <- sapply(paper$editor, shortName)
        if (!length(names))
          names <- paper$label
        if (!length(names))
          names <- sapply(paper$translator, shortName)
        names
      } 

  #     if (!missing(previous)) 
  #         cited <<- previous
  #     if (!missing(mode)) 
  #         mode <- match.arg(mode)
      numeric <- "numeric" %in% cite.style
      alphabetic <- "alphabetic" %in% cite.style
#       if (!length(cited) && length(unlist(papers$.index)))
#         .cites$has.labs <- TRUE
#       
#       if (numeric || alphabetic){ 
#         if (!.cites$has.labs || !any(names(bib) %in% cited)){  # 2nd condition for when another 
#           if (!length(sorting))                                    # BibEntry object is introduced
#             sorting <- switch(cite.style, authoryear = 'nyt', alphabetic = 'anyt', draft = 'debug', 'nty')
#           papers <- sort(papers, sorting = sorting, .bibstyle = cite.style, return.labs = TRUE)      
#         }
#       }
  #     keys <- unlist(strsplit(keys, " *, *"))
  #     if (!length(keys)) 
  #         return("")
      if (cite.style != .cites$sty)
        ClearLabs(cite.style)
      keys <- unlist(papers$key)
      n <- length(keys)
      cited <- names(.cites$indices)
      first <- !(keys %in% cited)
      if (cite.style != "numeric"){
        if (any(!names(bib) %in% names(.cites$labs))){ 
          # some entries in bib have note been seen before
          # note we use bib here instead of papers (the subset) in case
          # a possible "duplicate" in bib is cited in the future. we want to disambiguate this
          # By duplicate, I mean we want to distinguish Smith 2008a and Smith 2008b
          bibstyle <- switch(cite.style, authortitle = "authoryear", cite.style)
          bib <- sort(bib, sorting = "none", .bibstyle = bibstyle, return.labs = TRUE)      
          newinds <- bib$.index
          .labs <- newinds[keys]
          .cites$labs <- c(.cites$labs, newinds)
        }else{  # all entries in bib have been seen before, get the label from .cites env.
          .labs <- .cites$labs[keys]  
        }
      }else{
        first.ind <- which(first)
        if (length(first.ind)){
          shorthands <- unlist(papers$shorthand)
          max.ind <- suppressWarnings(sum(!is.na(as.numeric(.cites$labs))))
          newinds <- seq.int(max.ind+1L, length.out = length(first.ind))
          names(newinds) <- keys[first]
          if (length(shorthands))
            newinds[names(shorthands)] <- shorthands
          .cites$labs <- c(.cites$labs, newinds)
        }
        .labs <- .cites$labs[keys]
      }
      
      AddCite(keys, !identical(hyperlink, FALSE))
      year <- match(keys, names(.cites$indices))
      if (alphabetic || numeric){ 
        year <- structure(.labs, names = NULL)
      }else{
        year <- structure(unlist(sapply(papers$dateobj, 
                                        MakeAuthorYear()$DateFormatter)), names = NULL)

        if (any(.labs %in% letters)) # make sure labels are authoryear labels
          year <- paste0(year, .labs)
      }
  #     bibkeys <- unlist(bib$key)
  #     year <- match(keys, bibkeys)
  #     papers <- bib[year]
  
      if (textual || (!numeric && !alphabetic)){
          auth <- character(n)
          
          authorLists <- lapply(papers, authorList)
          lastAuthors <- NULL
          for (i in seq_along(keys)) {
              authors <- authorLists[[i]]
  #             if (identical(lastAuthors, authors)) 
  #                 auth[i] <- ""
  #             else {
            if (length(authors) > max.names && !(first[i]  && longnamesfirst)){
              authors <- authors[seq_len(max.names)]
              authors[length(authors)] <- paste0(authors[length(authors)], ", et al.")
            }else{
              if (length(authors) > 1L) 
                authors[length(authors)] <- paste("and", authors[length(authors)])
            }
            if (length(authors) > 2L)
              auth[i] <- paste(authors, collapse = ", ")
            else auth[i] <- paste(authors, collapse = " ")
  #               if (!(first[i] && longnamesfirst))
  #                 authors <- authors[min(length(authors), max.names)]
  #               if (length(authors) > 1L) 
  #                 authors[length(authors)] <- paste("and", authors[length(authors)])
  #               if (length(authors) > 2L) {
  #                 if (first[i] && longnamesfirst) 
  #                   auth[i] <- paste(authors, collapse = ", ")
  #                 else auth[i] <- paste(authors[1L], "et al.")
  #               }else auth[i] <- paste(authors, collapse = " ")
  #            }
  #            lastAuthors <- authors
          } # attempt to combine Smith 2008, Smith 2010 into Smith 2008, 2010.
  #         suppressauth <- which(!nzchar(auth))
  #         if (length(suppressauth)) {
  #             for (i in suppressauth) year[i - 1L] <- paste0(year[i - 
  #                 1L], bibpunct[6L], " ", year[i])
  #             auth <- auth[-suppressauth]
  #             year <- year[-suppressauth]
  #         }
      }
      make.hyper <- !identical(hyperlink, FALSE)
#       if (!is.null(before)) 
#         before <- paste0(before, " ")
#       if (!is.null(after)) 
#         after <- paste0(" ", after)
      if (textual) {
        if (numeric || alphabetic){
          result <- paste0(bibpunct[3L], before, year, after, bibpunct[4L])
        }else{
          result <- paste0(bibpunct[1L], before, year, after, bibpunct[2L])  
        }
        if (super && numeric && (!style %in% c("markdown", "html") || !make.hyper)) 
            result <- paste0(auth, "^{", result, "}")
        else if (!super || !numeric) result <- paste0(auth, " ", result)
       #   result <- paste(result, collapse = paste0(bibpunct[5L], 
      #        " "))
      }else if (numeric || alphabetic) {
        result <- year
#         result <- paste(year, collapse = paste0(bibpunct[5L], 
#             " "))
#         result <- paste0(bibpunct[3L], before, result, after, 
#             bibpunct[4L])
#         if (super && numeric) 
#             result <- paste0("^{", result, "}")
      }else {
          result <- paste0(auth, bibpunct[6L], " ", year)
#           result <- paste(result, collapse = paste0(bibpunct[5L], 
#               " "))
#           result <- paste0(bibpunct[1L], before, result, after, 
#               bibpunct[2L])
      }
      if (make.hyper){
        url = switch(hyperlink, to.bib = paste0("#bib-", gsub("[^_a-zA-Z0-9-]", "", keys)),
                     to.doc = sapply(papers, GetURL, 
                                     flds = c("url", "eprint", "doi"),
                                     to.bib = TRUE),
                     hyperlink)
        if (style == "html"){
          new.links <- if (any(first))
            paste(paste("<a id='cite-", gsub("[^_a-zA-Z0-9-]", "", keys[first]), "'></a>", sep = ""), collapse = "")
                       else ""
          result <- if (numeric && super && textual)
                      paste0(auth, "<sup><a href='", url, "'>", result, "</a></sup>")
                    else paste0("<a href='", url, "'>", result, "</a>")
        }else if(style == "markdown"){
          new.links <- if(any(first))
            paste(paste("<a name=cite-", gsub("[^_a-zA-Z0-9-]", "", keys[first]), "></a>", sep = ""), collapse = "")
          else ""
          result <- if (numeric && super && textual)
                      paste0(auth, "^[", result, "](", url, ")")
                    else paste0("[", result, "](", url, ")")
        }
      }
      result <- paste(result, collapse = paste0(bibpunct[5L], " "))
      if (!textual && (numeric || alphabetic)) {
        result <- paste0(bibpunct[3L], before, result, after, 
                         bibpunct[4L])
        if (super && numeric) 
          result <- paste0("^{", result, "}")
      }else if (!textual){
        result <- paste0(bibpunct[1L], before, result, after, 
                         bibpunct[2L])
      }
      if (make.hyper && style %in% c("html", "markdown"))
        result <- paste0(new.links, result)
      result
    })
    result
}

#' @return PrintBibliography: The formatted list of references.
#' @export
#' @aliases TextCite AutoCite Citep Citet
#' @details If \code{bib.style = "alphabetic"} or \code{bib.style = "numeric"},
#' then sorting needs to be done at the start of the document prior to using a cite function as
#' sorting is not done by the Printbibliography function for those styles (specifying
#' \code{sorting} in \code{.opts} is ignored in this case).  If no sorting is none,
#' the references are listed in the order they were cited in for those two styles.
#' 
#' If the \code{...} argument to NoCite is identical to \dQuote{*}, then all references
#' in \code{bib} are added to the bibliography without citations.
#' @seealso \code{\link{print.BibEntry}}, \code{\link{BibOptions}}, \code{\link[utils]{citeNatbib}}
#' @rdname Cite
PrintBibliography <- function(bib, .opts = list()){
  if (!length(bib))
    return(bib)
  keys <- unlist(bib$key)
  ind <- keys %in% names(.cites$indices)
  if (!any(ind)){
    message("You haven't cited any references in this bibliography yet.")
    return()
  }
  
  bibstyle <- if (length(.opts$bib.style))
                .opts$bib.style
              else .BibOptions$bib.style
  citestyle <- if (length(.opts$cite.style))
    .opts$cite.style
  else .BibOptions$cite.style
#   if (.opts$bib.style %in% c("alphabetic", "numeric"))
#     .opts$sorting  <- "none"

  
  bib <- bib[ind]
  # if bibstyle and citation style match, use citation labels, otherwise recompute them
  if (bibstyle == citestyle){
    if (bibstyle == "numeric"){
      if (length(bib) == length(.cites$labs)){
        bib <- bib[names(.cites$labs)]
        .opts$sorting <- "none"  
        bib$.index <- .cites$labs[keys[ind]]
      }
    }else bib$.index <- .cites$labs[keys[ind]]
  }

  if (length(.opts)){
    old.opts <- BibOptions(.opts)
    on.exit(BibOptions(old.opts))
  }
  print(bib)
#   env <- switch(.opts$bib.style, authoryear = MakeAuthorYear(), MakeBibLaTeX())
#   if (identical(tolower(BibOptions()$doc.type), "rhtml")){
#     if ()
#   }else if (identical(tolower(BibOptions()$doc.type), "rmd")){
#     if (.BibOptions$bib.style %in% c("authoryear", "authortitle")){
#       trace(toRd.BibEntry,{
#         env$.cites <- .cites$indices
#         env$fmtURL <- function(paper){
#           if (length(paper$url)) {
#             res <- paste0("\\url{", paper$url, "}")
#             if (length(paper$urldate)) {
#               fDate <- try(ProcessDate(paper$urldate, NULL), TRUE)
#               if (!is.null(fDate) && !inherits(fDate, "try-error")) 
#                 res <- paste0(res, " (visited on ", DateFormatter(fDate, 
#                                                                   TRUE), ")")
#             }
#             addPeriod(res)
#           }
#         }
#         
#           trace(env$fmtBAuthor,{
#             if (.cites[doc$key])
#               out <- paste0("<a name=#", doc$key, ">[", out, "](#cite-", doc$key, ")")
#           }, print = FALSE, at = 8L)
#       }, print = FALSE)
#     }
#   }
#   local({
#     
    
#  }, envir = env)
}

#' @export
#' @rdname Cite
Citep <- function(bib, ..., before = NULL, after = NULL, 
                  .opts = list()){
  kall <- match.call()
  kall[[1L]] <- as.name("Cite")
  kall$textual <- FALSE
  eval(kall)
}

#' @export
#' @rdname Cite
AutoCite <- function(bib, ..., before = NULL, after = NULL, 
                     .opts = list()){
  kall <- match.call()
  kall[[1L]] <- as.name("Cite")
  kall$textual <- FALSE
  eval(kall)
}

#' @export
#' @rdname Cite
Citet <- function(bib, ..., before = NULL, after = NULL, 
                  .opts = list()){
  kall <- match.call()
  kall[[1L]] <- as.name("Cite")
  kall$textual <- TRUE
  eval(kall)
}

#' @export
#' @rdname Cite 
TextCite <- function(bib, ..., before = NULL, after = NULL, 
                     .opts = list()){
  kall <- match.call()
  kall[[1L]] <- as.name("Cite")
  kall$textual <- TRUE
  eval(kall)
}

#' @keywords internal
ClearLabs <- function(sty){
  .cites$labs <- character(0)
  .cites$indices <- logical(0)
  .cites$sty <- sty
}

#' @rdname Cite
#' @aliases NoCite
#' @export
#' @keywords print methods
#' @return NoCite: no return value; invoked for its side-effect.
NoCite <- function(bib, ..., .opts = list()){
  if (length(.opts)){
    old.opts <- BibOptions(.opts)
    on.exit(BibOptions(old.opts))
  }
  if (identical(c(...), "*")){
    papers <- bib
  }else{
    papers <- suppressMessages(do.call(`[.BibEntry`, list(x = bib, ...)))  
  }
  
  if (!length(papers))
    return()
  keys <- unlist(papers$key)
  # n <- length(keys)
  # first <- !(keys %in% cited)
  if (.BibOptions$cite.style != "numeric"){
    if (!all(names(bib) %in% names(.cites$labs))){ 
      # some entries in bib have note been seen before
      # note we use bib here instead of papers (the subset) in case
      # a possible "duplicate" in bib is cited in the future. we want to disambiguate this
      # By duplicate, I mean we want to distinguish Smith 2008a and Smith 2008b
      bibstyle <- switch(.BibOptions$cite.style, authortitle = "authoryear", .BibOptions$cite.style)
      bib <- sort(bib, sorting = "none", .bibstyle = bibstyle, return.labs = TRUE)      
      .cites$labs <- c(.cites$labs, bib$.index)
    }
  }else{
    cited <- names(.cites$indices)
    first <- !(keys %in% cited)
    first.ind <- which(first)
    if (length(first.ind)){
      shorthands <- unlist(papers$shorthand)
      max.ind <- suppressWarnings(sum(!is.na(as.numeric(.cites$labs))))
      newinds <- seq.int(max.ind+1L, length.out = length(first.ind))
      names(newinds) <- keys[first]
      if (length(shorthands))
        newinds[names(shorthands)] <- shorthands
      .cites$labs <- c(.cites$labs, newinds)
    }
    .labs <- .cites$labs[keys]
  }
  AddCite(keys, !identical(.BibOptions$hyperlink, FALSE))
}



# local({
#   tmp$.duplicated <- FALSE
#   max.n <- 2
#   .bibstyle <- "alphabetical"
#   fmtBAuthor(tmp)
#   }, MakeBibLaTeX())
#   
# local({
#   tmp$.duplicated <- FALSE
#   max.n <- 2
#   bibstyle <- "alphabetical"
#   f <- function(){
#     print(bibstyle)
#   }
#   f()
#   })
#   
#   
#   env1 <- new.env()
#   env1$f <- function()print(foo)
#   local({
#   foo <- "hi"
#   f <- env1$f
#   f()
#   }, parent.frame())

