# Encode in a common format
#
# Format a BibEntry object in a pretty format.
#
# @param x - an object of class BibEntry
# @param style
# @return character vector containing formatted BibEntry object.
# @seealso \code{\link{print.BibEntry}}, \code{\link{BibEntry}}
#' @importFrom tools Rd2txt_options Rd2txt Rd2HTML Rd2latex
#' @S3method format BibEntry
#' @keywords internal
format.BibEntry <- function(x, style = .BibOptions$style, .bibstyle = .BibOptions$bib.style,
                             citation.bibtex.max = getOption("citation.bibtex.max", 1), .sort = TRUE,
                            .sorting = 'nty', enc = 'UTF-8', ...){
    old.opts <- BibOptions(list(style = .BibEntry_match_format_style(style),
                               return.ind = FALSE))
    on.exit(BibOptions(old.opts))
    ## style <- .BibEntry_match_format_style(style)
    ## ret.ind <- .BibOptions$return.ind
    ## .BibOptions$return.ind <- FALSE
    ## on.exit(.BibOptions$return.ind <- ret.ind)
    if (.sort && !style %in% c('html', 'text', 'latex', "markdown"))
      x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.labs = TRUE)

    .format_bibentry_via_Rd <- function(f){
        out <- file()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({
            tools::Rd2txt_options(saveopt)
            close(out)
        })
        x <- .BibEntry_expand_crossrefs(x)
        if (.sort)
          x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.labs = TRUE)
        res <- sapply(x, function(y) {
          rd <- toRd.BibEntry(y, .style = .bibstyle, .sorting = 'none')
          con <- textConnection(rd)
          on.exit(close(con))
          f(con, fragment = TRUE, out = out, outputEncoding = 'UTF-8', ...)
          paste(readLines(out, encoding = 'UTF-8'), collapse = "\n")
        })
        if (style == "html"){
          res <- sub("<code>([[:print:]]*)</code>", "<a id='bib-\\1'></a>", res, useBytes = TRUE)
          res <- if (.bibstyle == "alphabetic" || .bibstyle == "numeric")
            sub("^<p>([[:print:]]*\\])(</a>)?", "<p>\\1\\2<cite>", res, useBytes = TRUE)
          else if (.bibstyle == "draft") sub("^<p>([[:print:]]*</B>)",
                        "<p>\\1<cite>", res, useBytes = TRUE)
          else sub("^<p>", "<p><cite>", res, useBytes = TRUE)
          res <- sapply(res, function(x) if (grepl("<cite>", x, useBytes = TRUE))
                        paste0(x, "</cite></p>")
                      else  # XData or Set
                        paste0(x, "</p>"))
        }
        res
    }
    .format_bibentry_as_citation <- function(x) {
        bibtex <- length(x) <= citation.bibtex.max
        c(paste(strwrap(attr(x, "mheader")), collapse = "\n"),
            unlist(lapply(x, function(y) {
                paste(c(if (!is.null(y$header)) c(strwrap(y$header),
                  ""), if (!is.null(y$textVersion)) {
                  strwrap(y$textVersion, prefix = "  ")
                } else {
                  format(y, style = "text")
                }, if (bibtex) {
                  c(gettext("\nA BibTeX entry for LaTeX users is\n"),
                    paste0("  ", unclass(toBibtex(y))))
                }, if (!is.null(y$footer)) c("", strwrap(y$footer))),
                  collapse = "\n")
            })), paste(strwrap(attr(x, "mfooter")), collapse = "\n"))
    }
    out <- switch(style, text = .format_bibentry_via_Rd(tools::Rd2txt),
                  markdown = .format_bibentry_via_Rd(tools::Rd2txt),
        html = .format_bibentry_via_Rd(tools::Rd2HTML),
        latex = .format_bibentry_via_Rd(tools::Rd2latex),
        Biblatex = {
           x$.duplicated <- NULL
           unlist(lapply(x, function(y) paste(toBiblatex(y), collapse = "\n")))
        }, Bibtex = {
            x$.duplicated <- NULL
            unlist(lapply(x, function(y) paste(toBibtex(y), collapse = "\n")))
        }, textVersion = {
            out <- lapply(unclass(x), attr, "textVersion")
            out[!sapply(out, length)] <- ""
            unlist(out)
        }, citation = .format_bibentry_as_citation(x), R = {
          x$.duplicated <- NULL
          .format_BibEntry_as_R_code(x,
            ...)
          }, yaml = {
          x$.duplicated <- NULL
          .format_BibEntry_as_yaml(x,
            ...)
          })
    as.character(out)
}
