#' Encode in a common format
#' 
#' Format a BibEntry object in a pretty format.
#' 
#' @param x - an object of class BibEntry
#' @param style
#' @return character vector containing formatted BibEntry object.
#' @importFrom utils .format_bibentry_as_R_code
#' @method format BibEntry
#' @keywords internal
#' @seealso \code{\link{print.BibEntry}}, \code{\link{BibEntry}}
format.BibEntry <- function(x, style = "text", .bibstyle = BibOptions()$bib.style, 
                             citation.bibtex.max = getOption("citation.bibtex.max", 1), sort = TRUE, 
                            .sorting = 'nty', enc = 'UTF-8', ...){
    style <- .BibEntry_match_format_style(style)
    ret.ind <- .BibOptions$return.ind
    .BibOptions$return.ind <- FALSE
    if (sort && !style %in% c('html', 'text', 'latex')) 
      x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.ind = TRUE)

    .format_bibentry_via_Rd <- function(f){
        out <- file()
      #  browser()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({
            tools::Rd2txt_options(saveopt)
            close(out)
        })
        x <- .BibEntry_expand_crossrefs(x)
        if (sort) 
          x <- sort(x, .bibstyle = .bibstyle, sorting = .sorting, return.ind = TRUE)
        sapply(x, function(y) {

            rd <- toRd.BibEntry(y, style = .bibstyle, .sorting = 'none')
            con <- textConnection(rd)
            on.exit(close(con))
            f(con, fragment = TRUE, out = out, outputEncoding = 'UTF-8', ...)
            paste(readLines(out, encoding = 'UTF-8'), collapse = "\n")
        })
#         browser()
#         sapply(rd[1], function(y){
#           con <- textConnection(y)
#           on.exit(close(con))
#           f(con, fragment = TRUE, out = out, encoding = 'UTF-8', ...)
#            paste(readLines(out), collapse = "\n")
#         }, simplify = FALSE, USE.NAMES = FALSE)
#        rd <- toRd.BibEntry(.BibEntry_expand_crossrefs(x), style = .bibstyle, .sorting = 'none') 
#        lenrd <- length(rd)  
#        for (i in seq_len(lenrd-1)){
#           con <- textConnection(rd[i])
#           f(con, fragment = TRUE, out = out, encoding = 'UTF-8', ...)
#           cat(paste(readLines(out, encoding = 'UTF-8'), collapse = "\n"), '\n\n')
#           close(con)
#        }
#       con <- textConnection(rd[lenrd])
#       f(con, fragment = TRUE, out = out, encoding = 'UTF-8', ...)
#       cat(encodeString(paste(readLines(out, encoding = 'UTF-8'), collapse = "\n")))
#       close(con)

    }
    .format_bibentry_as_citation <- function(x) {
        bibtex <- length(x) <= citation.bibtex.max
        c(paste(strwrap(attr(x, "mheader")), collapse = "\n"), 
            unlist(lapply(x, function(y) {
                paste(c(if (!is.null(y$header)) c(strwrap(y$header), 
                  ""), if (!is.null(y$textVersion)) {
                  strwrap(y$textVersion, prefix = "  ")
                } else {
                  format(y)
                }, if (bibtex) {
                  c(gettext("\nA BibTeX entry for LaTeX users is\n"), 
                    paste0("  ", unclass(toBibtex(y))))
                }, if (!is.null(y$footer)) c("", strwrap(y$footer))), 
                  collapse = "\n")
            })), paste(strwrap(attr(x, "mfooter")), collapse = "\n"))
    }
    out <- switch(style, text = .format_bibentry_via_Rd(tools::Rd2txt), 
        html = .format_bibentry_via_Rd(tools::Rd2HTML), latex = .format_bibentry_via_Rd(tools::Rd2latex), 
        Bibtex = {
            unlist(lapply(x, function(y) paste(toBibtex(y), collapse = "\n")))
        }, textVersion = {
            out <- lapply(unclass(x), attr, "textVersion")
            out[!sapply(out, length)] <- ""
            unlist(out)
        }, citation = .format_bibentry_as_citation(x), R = .format_bibentry_as_R_code(x, 
            ...))
    .BibOptions$return.ind <- ret.ind
    as.character(out)
}