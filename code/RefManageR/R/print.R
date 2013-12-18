print.BibEntry <- function (x, style = "text", .bibstyle = NULL, ...) 
{
    style <- .BibEntry_match_format_style(style)
    if (style == "R") {
        writeLines(format(x, "R", collapse = TRUE, ...))
    }
    else if (length(x)) {
        y <- format(x, style, .bibstyle, ...)
        if (style == "citation") {
            n <- length(y)
            if (nzchar(header <- y[1L])) 
                header <- c("", header, "")
            if (nzchar(footer <- y[n])) 
                footer <- c("", footer, "")
            writeLines(c(header, paste(y[-c(1L, n)], collapse = "\n\n"), 
                footer))
        }
        else {
            writeLines(paste(y, collapse = "\n\n"))
        }
    }
    invisible(x)
}

format.BibEntry <- function (x, style = "text", .bibstyle = NULL, citation.bibtex.max = getOption("citation.bibtex.max", 
    1), sort = FALSE, ...){
   
    style <- .BibEntry_match_format_style(style)
    if (sort) 
        x <- sort(x, .bibstyle = .bibstyle)
    x$.index <- as.list(seq_along(x))
    .format_bibentry_via_Rd <- function(f) {
        out <- file()
      #  browser()
        saveopt <- tools::Rd2txt_options(width = getOption("width"))
        on.exit({
            tools::Rd2txt_options(saveopt)
            close(out)
        })
        sapply(.BibEntry_expand_crossrefs(x), function(y) {
          #browser()
            rd <- toRd.BibEntry(y, style = .bibstyle)
            con <- textConnection(rd)
            on.exit(close(con))
            f(con, fragment = TRUE, out = out, encoding = 'UTF-8', ...)
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