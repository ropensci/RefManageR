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
#' @param bib a \code{BibEntry} or \code{bibentry} object
#' @param ... passed to \code{\link{SearchBib}} for indexing into bib.  A character vector of
#' keys, for example.
#' @param textual logical; if TRUE, a \dQuote{textual} citation is produced, i.e.
#' what is produced by \\citet in \code{natbib} and \\textcite in \code{BibLaTeX}; otherwise,
#' a parenthetical citation as \\citep and \\autocite.
#' @param before string; optional text to display before the citation.
#' @param after string; optional text to display after the citation.
#' @param .opts list; See the relevant section in \code{\link{BibOptions}} for a description
#' of all valid options for these functions.
#' @details See the package vignettes and execute the examples below.
#' @return For the cite functions: a character string containing the citation
#' @rdname Cite
#' @export
#' @aliases PrintBibliography
#' @examples
#' file <- system.file("Bib", "biblatexExamples.bib", package = "RefManageR")
#' BibOptions(check.entries = FALSE)
#' bib <- ReadBib(file)
#' Citet(bib, 12)
#' Citep(bib, c("loh", "geer"), .opts = list(cite.style = "numeric"), before = "see e.g., ")
#' Citet(bib, "loh", .opts = list(cite.style = "numeric", super = TRUE))
#' AutoCite(bib, eprinttype = "arxiv", .opts = list(cite.style = "authoryear"))
#' AutoCite(bib, eprinttype = "arxiv", .opts = list(cite.style = "pandoc"))
#' Citep(bib, author = "kant")
#' ## shorthand field in both entries gets used for numeric and alphabetic labels
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
#' old.dir <- setwd(tdir <- tempdir())
#' doc <- system.file("Rmd", "TestRmd.Rmd", package = "RefManageR")
#' file.show(doc)
#' tmpfile <- tempfile(fileext = ".html", tmpdir = tdir)
#' knit2html(doc, tmpfile)
#' browseURL(tmpfile)
#'
#' doc <- system.file("Rhtml", "TestAuthorYear.Rhtml", package = "RefManageR")
#' file.show(doc)
#' tmpfile <- tempfile(fileext = ".html", tmpdir = tdir)
#' knit2html(doc, tmpfile)
#' browseURL(tmpfile)
#' setwd(old.dir)
#' unlink(tdir)
#' }
Cite <- function(bib, ..., textual = FALSE, before = NULL, after = NULL,
                 .opts = list()){
    if (length(.opts)){
      old.opts <- BibOptions(.opts)
      on.exit(BibOptions(old.opts))
    }

    if (identical(class(bib), "bibentry"))
      bib <- as.BibEntry(bib)

    result <- with(BibOptions(), {
      style <- .BibEntry_match_format_style(style)
      papers <- suppressMessages(do.call(`[.BibEntry`, list(x = bib, ...)))
      keys <- unlist(papers$key)
      if (!length(papers))
        return("")
      if (cite.style == "pandoc"){
        result <- paste0(paste0("@", names(papers)), collapse = paste0(bibpunct[5L],
                                                         " "))
        result <- paste0(before, result, after)
        if (textual)
          result <- paste0(bibpunct[3L], result,
                             bibpunct[4L])
        AddCite(keys, FALSE)
        result
      }else{
        shortName <- function(person){
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

        numeric <- "numeric" %in% cite.style
        alphabetic <- "alphabetic" %in% cite.style
        if (cite.style != .cites$sty)
          ClearLabs(cite.style)
        n <- length(papers)
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
          first.ind <- if (!length(.cites$labs))  # cite.style has changed, labs have been reset
                         seq_along(papers)
                       else which(first | !keys %in% names(.cites$labs))
          if (length(first.ind)){
            shorthands <- unlist(papers$shorthand)
            max.ind <- suppressWarnings(sum(!is.na(as.numeric(.cites$labs))))
            newinds <- seq.int(max.ind+1L, length.out = length(first.ind))
            names(newinds) <- keys[first.ind]
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

        if (textual || (!numeric && !alphabetic)){
            auth <- character(n)
            authorLists <- lapply(papers, authorList)
            lastAuthors <- NULL
            for (i in seq_len(n)) {
                authors <- authorLists[[i]]
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
            }
    # attempt to combine Smith 2008, Smith 2010 into Smith 2008, 2010.
    #         suppressauth <- which(!nzchar(auth))
    #         if (length(suppressauth)) {
    #             for (i in suppressauth) year[i - 1L] <- paste0(year[i -
    #                 1L], bibpunct[6L], " ", year[i])
    #             auth <- auth[-suppressauth]
    #             year <- year[-suppressauth]
    #         }
        }
        make.hyper <- !identical(hyperlink, FALSE)

        if (textual) {
          if (numeric || alphabetic){
            result <- paste0(bibpunct[3L], before, year, after, bibpunct[4L])
          }else{
            result <- paste0(bibpunct[1L], before, year, after, bibpunct[2L])
          }
          if (super && numeric && (!style %in% c("markdown", "html") || !make.hyper))
              result <- paste0(auth, "^{", result, "}")
          else if (!super || !numeric) result <- paste0(auth, " ", result)
        }else if (numeric || alphabetic) {
          result <- year
        }else {
            result <- paste0(auth, bibpunct[6L], " ", year)
        }
        if (make.hyper){
            url = switch(hyperlink, to.bib = paste0("#bib-", gsub("[^_a-zA-Z0-9-]", "", keys,
                                        useBytes = TRUE)),
                       to.doc = sapply(papers, GetURL,
                                       flds = c("url", "eprint", "doi"),
                                       to.bib = TRUE),
                       hyperlink)
          if (style == "html"){
            new.links <- if (any(first))
                             paste(paste("<a id='cite-", gsub("[^_a-zA-Z0-9-]", "", keys[first],
                                                 useBytes = TRUE), "'></a>", sep = ""),
                                   collapse = "")
                         else ""
            result <- if (numeric && super && textual)
                        paste0(auth, "<sup><a href='", url, "'>", result, "</a></sup>")
                      else paste0("<a href='", url, "'>", result, "</a>")
          }else if(style == "markdown"){
            new.links <- if(any(first))
                             paste(paste("<a name=cite-", gsub("[^_a-zA-Z0-9-]", "", keys[first],
                                                 useBytes = TRUE), "></a>", sep = ""),
                                   collapse = "")
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
      }  # end else for if cite.style == "pandoc"
    })  # end with for BibOptions
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
  if (identical(class(bib), "bibentry"))
    bib <- as.BibEntry(bib)
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
  style <- if (length(.opts$style))
                .opts$style
              else .BibOptions$style

  bib <- bib[[ind]]
  # if bibstyle and citation style match, use citation labels, otherwise recompute them
  if (bibstyle == citestyle){
    if (bibstyle == "numeric"){
      if (length(bib) == length(.cites$labs)){
        bib <- bib[[names(.cites$labs)]]
        .opts$sorting <- "none"
#         labs <- .cites$labs
#         bib.labs <- labs[order(match(keys, names(labs)))]
#         bib <- bib[names(bib.labs)] # sort
        bib$.index <- structure(.cites$labs, names = NULL)
      }
    }else bib$.index <- .cites$labs[keys[ind]]
  }

  if (length(.opts)){
    old.opts <- BibOptions(.opts)
    on.exit(BibOptions(old.opts))
  }

  if (style == "yaml"){
    cat("\n---\nnocite:",
    sQuote(paste0(paste0("@", names(.cites$indices)), collapse = ", ")))
    cat("\n...  \n\n")
  }
  print(bib)
}

#' @export
#' @rdname Cite
Citep <- function(bib, ..., before = NULL, after = NULL,
                  .opts = list()){
  ## kall <- match.call()
  ## kall[[1L]] <- as.name("Cite")
  ## kall$textual <- FALSE
  ## eval(kall)
  Cite(bib, ..., textual = FALSE, before = before, after = after, .opts = .opts)
}

#' @export
#' @rdname Cite
AutoCite <- function(bib, ..., before = NULL, after = NULL,
                     .opts = list()){
  ## kall <- match.call()
  ## kall[[1L]] <- as.name("Cite")
  ## kall$textual <- FALSE
  ## eval(kall)
  Cite(bib, ..., textual = FALSE, before = before, after = after, .opts = .opts)
}

#' @export
#' @rdname Cite
Citet <- function(bib, ..., before = NULL, after = NULL,
                  .opts = list()){
  ## kall <- match.call()
  ## kall[[1L]] <- as.name("Cite")
  ## kall$textual <- TRUE
  ## eval(kall)
  Cite(bib, ..., textual = TRUE, before = before, after = after, .opts = .opts)
}

#' @export
#' @rdname Cite
TextCite <- function(bib, ..., before = NULL, after = NULL,
                     .opts = list()){
  ## kall <- match.call()
  ## kall[[1L]] <- as.name("Cite")
  ## kall$textual <- TRUE
  ## eval(kall)
  Cite(bib, ..., textual = TRUE, before = before, after = after, .opts = .opts)
}

#' @keywords internal
ClearLabs <- function(sty){
  .cites$labs <- character(0)
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
  invisible(NULL)
}
