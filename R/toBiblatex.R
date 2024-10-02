#' Convert BibEntry objects to BibTeX or BibLaTeX
#'
#' toBiblatex converts a BibEntry object to character vectors with BibLaTeX markup.  toBibtex will convert a BibEntry object
#' to character vectors with BibTeX markup, converting some BibLaTeX fields and all entry types that are not supported
#' by BibTeX to ones that are supported.
#' @param object an object of class BibEntry to be converted
#' @param note.replace.field a character vector of BibLaTeX fields.  When converting an entry to BibTeX, the first field in the
#' entry that matches one specified in this vector will be added to the note field, \emph{if} the note field is not already
#' present
#' @param extra.fields character vector; fields that are not supported in standard BibTeX styles are by default dropped
#' in the result return by the toBibtex function.
#' Any fields specified in extra.fields will \emph{not} be dropped if present in an entry.
#' @param encoded.names.to.latex if \code{TRUE} (the default) then name list fields
#' such as \sQuote{author} and \sQuote{editor} will have non-ASCII characters
#' translated to LaTeX escape sequences by \code{\link[dplR]{latexify}}.
#' @param ... ignored
#' @export
#' @return an object of class \dQuote{Bibtex} - character vectors where each element holds one line of a BibTeX or BibLaTeX file
#' @details toBiblatex converts the BibEntry object to a vector containing the corresponding BibLaTeX file, it ensures the name
#' list fields (e.g. author and editor) are formatted properly to be read by bibtex and biber and otherwise prints all fields
#' as is, thus it is similar to \code{\link{toBibtex}}.
#'
#' toBibtex will attempt to convert BibLaTeX entries to a format that can be read by bibtex.  Any fields not supported by
#' bibtex are dropped unless they are specified in \code{extra.fields}.  The fields below, if they are present, are converted
#' as described and added to a bibtex supported field, unless that field is already present.
#' \itemize{
#' \item date - The \code{date} field, if present will be truncated
#' to a year and added to the \code{year} field, if it is not already present. If a month is specified with the date, it will
#' be added to the \code{month} field.
#' \item journaltitle - Will be changed to journal, if it is not already present
#' \item location - Will be changed to address
#' \item institution - Converted to \code{school} for thesis entries
#' \item sortkey - Converted to \code{key}
#' \item maintitle - Converted to \code{series}
#' \item issuetitle - Converted to \code{booktitle}
#' \item eventtitle - Converted to \code{booktitle}
#' \item eprinttype - Converted to \code{archiveprefix} (for arXiv references)
#' \item eprintclass - Converted to \code{primaryclass} (for arXiv references)
#' }
#'
#' If no \code{note} field is present, the note.replace.field can be used to specified BibLaTeX fields that can be looked for
#' and added to the note field if they are present.
#'
#' BibLaTeX entry types that are not supported by bibtex are converted by toBibtex as follows
#' "mvbook" = "Book", "bookinbook" = "InBook", "suppbook" = "InBook",
#' \itemize{
#' \item MvBook,Collection,MvCollection,Reference,MvReference,Proceedings,MvProceedings,Periodical - to Book
#' \item BookInBook,SuppBook,InReference,SuppPeriodical - to InBook
#' \item report,patent - to TechReport
#' \item SuppCollection - to InCollection
#' \item thesis - to MastersThesis if \code{type = mathesis}, else to PhdThesis
#' \item \emph{rest} - to Misc
#' }
#' @seealso \code{\link{toBibtex}}, \code{\link{BibEntry}}, \code{\link{print.BibEntry}}
#' @author McLean, M. W. \email{mathew.w.mclean@@gmail.com}
#' @importFrom tools parseLatex deparseLatex latexToUtf8
#' @keywords database IO utilities
#' @aliases toBibtex.BibEntry toBibtex
#' @examples
#' if (requireNamespace("bibtex")) {
#'     file.name <- system.file("Bib", "biblatexExamples.bib", package="RefManageR")
#'     bib <- suppressMessages(ReadBib(file.name))
#'     toBiblatex(bib[70:72])
#'     toBibtex(bib[70:72])
#' }
toBiblatex <- function(object, encoded.names.to.latex = TRUE, ...){
    format_bibentry1 <- function(object) {
      object <- unclass(object)[[1L]]
      rval <- paste0("@", attr(object, "bibtype"), "{", attr(object,
          "key"), ",")
      if (encoded.names.to.latex) {
        nl.ind <- which(names(object) %in% .BibEntryNameList)
        for (i in nl.ind)
          object[i] <- EncodedNameListToLaTeX(object[[i]])
      }
      rval <- c(rval, vapply(names(object), function(n) paste0("  ",
          n, " = {", object[[n]], "},"), ""), "}", "")
      return(rval)
    }
    if (length(object)) {
        object$.index <- NULL
        rval <- head(unlist(lapply(object, format_bibentry1)),
            -1L)
    }
    else rval <- character()
    class(rval) <- "Bibtex"
    rval
}

#' Wrapper for dplr::latexify that returns original
#' text if translation to LaTeX fails
#' @noRd
#' @seealso  \url{https://github.com/ropensci/RefManageR/issues/106}
EncodedNameListToLaTeX <- function(name.list, encoding = "UTF-8")
{
  formatted.text <- format_author(name.list)
  out <- latexify(formatted.text, encoding, doublebackslash = FALSE)
  if (grepl("^[{]?[?]", out))
    return(formatted.text)
  return(out)
}

#' Modified version of dplR::latexify
#' @importFrom stringi stri_trans_nfc stri_trans_nfd
#' @importFrom R.utils captureOutput
#' @author Andy Bunn
latexify <- function(x, 
                     doublebackslash = FALSE, 
                     dashdash = TRUE, 
                     quotes = c("straight", "curved"), 
                     packages = c("fontenc", "textcomp")) 
{
  y <- as.character(x)
  encBytes <- Encoding(y) == "bytes"
  if (any(encBytes)) {
    y[encBytes] <- captureOutput(cat(y[encBytes], sep = "\n"))
  }
  y <- stri_trans_nfd(y)
  Letters <- paste0(c(LETTERS, letters), collapse = "")
  fontenc <- "fontenc" %in% packages
  textcomp <- "textcomp" %in% packages
  eurosym <- "eurosym" %in% packages
  straightQuotes <- match.arg(quotes) == "straight"
  y <- gsub("(?![[:space:]])[[:cntrl:]]", "", y, perl = TRUE)
  y <- gsub("[[:space:]]+", " ", y)
  substitutions <- list(c("\\^", "\\\\textasciicircum{}"), 
                        c("~", "\\\\textasciitilde{}"), c("<", "\\\\textless{}"), 
                        c(">", "\\\\textgreater{}"), c("\\|", "\\\\textbar{}"), 
                        c("([#$%&_])", "\\\\\\1"), if (isTRUE(dashdash)) {
                          c("-", "\\\\mbox{-}")
                        }, if (textcomp && straightQuotes) {
                          c("'", "\\\\textquotesingle{}")
                        }, if (textcomp && straightQuotes) {
                          c("`", "\\\\textasciigrave{}")
                        }, c("\"", if (fontenc && straightQuotes) {
                          "\\\\textquotedbl{}"
                        } else {
                          "\\\\textquotedblright{}"
                        }), c("/", "\\\\slash{}"))
  substitutions <- substitutions[!vapply(substitutions, is.null, 
                                         logical(1))]
  substitutions <- c(substitutions, list(c("Ĳ", "\\\\IJ{}"), 
                                         c("ĳ", "\\\\ij{}"), c("Ǳ", "DZ"), c("ǲ", "Dz"), c("ǳ", 
                                                                                           "dz"), c("Ǆ", "DŽ"), c("ǅ", "Dž"), c("ǆ", 
                                                                                                                                  "dž"), c("Ǉ", "LJ"), c("ǈ", "Lj"), c("ǉ", "lj"), 
                                         c("Ǌ", "NJ"), c("ǋ", "Nj"), c("ǌ", "nj"), c("ﬀ", 
                                                                                     "ff"), c("ﬁ", "fi"), c("ﬂ", "fl"), c("ﬃ", "ffi"), 
                                         c("ﬄ", "ffl"), c("ﬅ", "ſt"), c("ﬆ", "st")))
  above <- list(diaeresis = c("̈", "\""), acute = c("́", 
                                                    "'"), dotabove = c("̇", "."), macron = c("̄", "="), 
                circumflex = c("̂", "^"), grave = c("̀", "`"), tilde = c("̃", 
                                                                         "~"), doubleacute = c("̋", "H"), ringabove = c("̊", 
                                                                                                                        "r"), breve = c("̆", "u"), caron = c("̌", "v"), 
                invbreve = c("̑", "newtie"))
  below <- list(macronbelow = c("̱", "b"), cedilla = c("̧", 
                                                       "c"), dotbelow = c("̣", "d"), tie = c("͡", "t"), ogonek = c("̨", 
                                                                                                                   "k"))
  accents <- c(above, below)
  command <- paste0("\\\\[", Letters, "]+|\\\\.")
  combining <- paste0(vapply(accents, "[", character(1), 1), 
                      collapse = "")
  accPre <- paste0("(", command, "|.)({})?(?<![", combining, 
                   "])")
  accPost <- paste0("(?![", combining, "])")
  aboveInCode <- vapply(above, "[", character(1), 1)
  ijPattern <- paste0("([ij])", aboveInCode, accPost)
  otherPattern <- paste0(accPre, aboveInCode, accPost)
  aboveOutCode <- vapply(above, "[", character(1), 2)
  ijReplacement <- paste0("\\\\", aboveOutCode, "{\\\\\\1}")
  otherReplacement <- paste0("\\\\", aboveOutCode, "{\\1}")
  belowInCode <- vapply(below, "[", character(1), 1)
  belowPattern <- paste0(accPre, belowInCode, accPost)
  belowOutCode <- vapply(below, "[", character(1), 2)
  belowReplacement <- paste0("\\\\", belowOutCode, "{\\1}")
  circPre <- paste0("(", command, "({([^}]|\\\\})+})?|.)({})?")
  circPattern <- paste0(circPre, "⃝", accPost)
  circReplacement <- "\\\\textcircled{\\1}"
  substitutions <- c(substitutions, lapply(lapply(mapply(list, 
                                                         list(as.name("c")), c(ijPattern, otherPattern, belowPattern, 
                                                                               circPattern), c(ijReplacement, otherReplacement, 
                                                                                               belowReplacement, circReplacement), SIMPLIFY = FALSE), 
                                                  as.call), eval))
  substitutions <- c(substitutions, list(c("¡", "\\\\textexclamdown{}"), 
                                         c("£", "\\\\pounds{}"), c("§", "\\\\S{}"), c("©", 
                                                                                      "\\\\copyright{}"), c("ª", "\\\\textordfeminine{}"), 
                                         c("®", "\\\\textregistered{}"), c("¶", "\\\\P{}"), 
                                         c("·", "\\\\textperiodcentered{}"), c("º", "\\\\textordmasculine{}"), 
                                         c("¿", "\\\\textquestiondown{}"), c("–", "\\\\textendash{}"), 
                                         c("—", "\\\\textemdash{}"), c("‘", "\\\\textquoteleft{}"), 
                                         c("’", "\\\\textquoteright{}"), c("“", "\\\\textquotedblleft{}"), 
                                         c("”", "\\\\textquotedblright{}"), c("†", "\\\\dag{}"), 
                                         c("‡", "\\\\ddag{}"), c("•", "\\\\textbullet{}"), 
                                         c("…", "\\\\dots{}"), c("™", "\\\\texttrademark{}"), 
                                         c("␣", "\\\\textvisiblespace{}"), c("Æ", "\\\\AE{}"), 
                                         c("æ", "\\\\ae{}"), c("Œ", "\\\\OE{}"), c("œ", "\\\\oe{}"), 
                                         c("Ø", "\\\\O{}"), c("ø", "\\\\o{}"), c("Ł", "\\\\L{}"), 
                                         c("ł", "\\\\l{}"), c("ẞ", "\\\\ifdefined\\\\XeTeXrevision\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\ifdefined\\\\directlua\\\\iffontchar\\\\font\"1E9E\\\\symbol{\"1E9E}\\\\else\\\\SS\\\\fi\\\\else\\\\SS\\\\fi\\\\fi{}"), 
                                         c("ß", "\\\\ss{}"), c("ſ", "\\\\ifdefined\\\\XeTeXrevision\\\\symbol{\"017F}\\\\else\\\\ifdefined\\\\directlua\\\\symbol{\"017F}\\\\else{\\\\fontencoding{TS1}\\\\selectfont s}\\\\fi\\\\fi{}")))
  substitutions <- c(substitutions, list(c("Ð", "\\\\DH{}"), 
                                         c("ð", "\\\\dh{}"), c("Đ", "\\\\DJ{}"), c("đ", "\\\\dj{}"), 
                                         c("Ŋ", "\\\\NG{}"), c("ŋ", "\\\\ng{}"), c("Þ", "\\\\TH{}"), 
                                         c("þ", "\\\\th{}"), c("«", "\\\\guillemotleft{}"), 
                                         c("»", "\\\\guillemotright{}"), c("‚", "\\\\quotesinglbase{}"), 
                                         c("„", "\\\\quotedblbase{}"), c("‹", "\\\\guilsinglleft{}"), 
                                         c("›", "\\\\guilsinglright{}")))
  substitutions <- c(substitutions, list(c("­", "\\\\-"), c("​", "\\\\hspace{0pt}"), c("∗", "\\\\textasteriskcentered{}"), 
                                         c("‖", "\\\\textbardbl{}"), c("◯", "\\\\textbigcircle{}"), 
                                         c("␢", "\\\\textblank{}"), c("¦", "\\\\textbrokenbar{}"), 
                                         c("⁒", "\\\\textdiscount{}"), c("℮", "\\\\textestimated{}"), 
                                         c("‽", "\\\\textinterrobang{}"), c("⸘", "\\\\textinterrobangdown{}"), 
                                         c("№", "\\\\textnumero{}"), c("◦", "\\\\textopenbullet{}"), 
                                         c("‰", "\\\\textperthousand{}"), c("‱", "\\\\textpertenthousand{}"), 
                                         c("℞", "\\\\textrecipe{}"), c("※", "\\\\textreferencemark{}"), 
                                         c("˷", "\\\\texttildelow{}"), c("←", "\\\\textleftarrow{}"), 
                                         c("↑", "\\\\textuparrow{}"), c("→", "\\\\textrightarrow{}"), 
                                         c("↓", "\\\\textdownarrow{}"), c("〈", "\\\\textlangle{}"), 
                                         c("〉", "\\\\textrangle{}"), c("〚", "\\\\textlbrackdbl{}"), 
                                         c("〛", "\\\\textrbrackdbl{}"), c("⁅", "\\\\textlquill{}"), 
                                         c("⁆", "\\\\textrquill{}"), c("℗", "\\\\textcircledP{}"), 
                                         c("℠", "\\\\textservicemark{}"), c("℃", "\\\\textcelsius{}"), 
                                         c("℧", "\\\\textmho{}"), c("µ", "\\\\textmu{}"), c("Ω", 
                                                                                            "\\\\textohm{}"), c("฿", "\\\\textbaht{}"), c("¢", 
                                                                                                                                          "\\\\textcent{}"), c("₡", "\\\\textcolonmonetary{}"), 
                                         c("¤", "\\\\textcurrency{}"), c("₫", "\\\\textdong{}"), 
                                         c("€", if (eurosym) "\\\\euro{}" else "\\\\texteuro{}"), 
                                         c("₲", "\\\\textguarani{}"), c("₤", "\\\\textlira{}"), 
                                         c("₦", "\\\\textnaira{}"), c("₱", "\\\\textpeso{}"), 
                                         c("₩", "\\\\textwon{}"), c("¥", "\\\\textyen{}"), 
                                         c("˝", "\\\\textacutedbl{}"), c("´", "\\\\textasciiacute{}"), 
                                         c("¸", "\\\\c{}"), c("˘", "\\\\textasciibreve{}"), 
                                         c("ˇ", "\\\\textasciicaron{}"), c("¨", "\\\\textasciidieresis{}"), 
                                         c("¯", "\\\\textasciimacron{}"), c("°", "\\\\textdegree{}"), 
                                         c("÷", "\\\\textdiv{}"), c("¼", "\\\\textonequarter{}"), 
                                         c("½", "\\\\textonehalf{}"), c("¾", "\\\\textthreequarters{}"), 
                                         c("×", "\\\\texttimes{}"), c("±", "\\\\textpm{}"), 
                                         c("¹", "\\\\textonesuperior{}"), c("²", "\\\\texttwosuperior{}"), 
                                         c("³", "\\\\textthreesuperior{}"), c("⁄", "\\\\textfractionsolidus{}"), 
                                         c("√", "\\\\textsurd{}"), c("¬", "\\\\textlnot{}"), 
                                         c("−", "\\\\textminus{}")))
  tmp <- paste0("(\\\\[", Letters, "]+){}")
  substitutions <- c(substitutions, list(c(paste0(tmp, "(?=$|[[:digit:],.?!;:\\\\}+*/-])"), 
                                           "\\1"), c(paste0(tmp, "(?! )"), "\\1 ")))
  for (subst in substitutions) {
    y <- gsub(subst[1], subst[2], y, perl = TRUE)
  }
  if (isTRUE(doublebackslash)) {
    y <- gsub("\\", "\\\\", y, fixed = TRUE)
  }
  stri_trans_nfc(y)
}

