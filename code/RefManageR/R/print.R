print.BibEntry <- function (x, style = "text", .bibstyle = .BibOptions$bib.style, ...) 
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