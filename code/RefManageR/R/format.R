format.BibEntry <- function (x, style = "text", .bibstyle = NULL, citation.bibtex.max = getOption("citation.bibtex.max", 
                                                                                                  1), sort = FALSE, ...) {
  style <- .bibentry_match_format_style(style)
  if (sort) 
    x <- sort(x, .bibstyle = .bibstyle)
  x$.index <- as.list(seq_along(x))
  .format_bibentry_via_Rd <- function(f) {
    out <- file()
    saveopt <- tools::Rd2txt_options(width = getOption("width"))
    on.exit({
      tools::Rd2txt_options(saveopt)
      close(out)
    })
    sapply(.bibentry_expand_crossrefs(x), function(y) {
      rd <- tools::toRd(y, style = .bibstyle)
      con <- textConnection(rd)
      on.exit(close(con))
      f(con, fragment = TRUE, out = out, ...)
      paste(readLines(out), collapse = "\n")
    })
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
  as.character(out)
}