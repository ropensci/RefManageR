context("Cite functions")
# rm(list = ls(all=TRUE))
unloadNamespace("RefManageR")
library(RefManageR)

old.opts <- BibOptions(check.entries = FALSE, style = "markdown", bib.style = "numeric", cite.style = "numeric")
test_that("BibOptions returns changed options correctly", {
  expect_identical(old.opts, list(check.entries = "error", style = "text",
                                  bib.style = "numeric", cite.style = "authoryear"))
})

bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                           package = "RefManageR"), check = FALSE)

expect_identical(Citet(bib, 11, .opts = list(longnamesfirst = FALSE)),
                 "<a name=cite-glashow></a>[Glashow [1]](#bib-glashow)")
expect_identical(AutoCite(bib, "baez/online", before = "e.g., "),
 "<a name=cite-baezonline></a>[e.g., [2](http://arxiv.org/abs/math/0307200v3)]")
expect_identical(AutoCite(bib, "baez/online", before = "e.g., "),
                 "[e.g., [2](http://arxiv.org/abs/math/0307200v3)]")
expect_identical(AutoCite(bib, author = "kant"),
  "<a name=cite-kantkpv></a><a name=cite-kantku></a>[[KpV](#bib-kantkpv); [KU](#bib-kantku)]")
expect_identical(Citet(bib, bibtype = "Report", .opts = list(hyperlink = "to.doc", super = TRUE)),
 "<a name=cite-chiu></a><a name=cite-padhye></a>Chiu and Chow^[[3]](#bib-chiu); Padhye, Firoiu, and Towsley^[[4]](#bib-padhye)")
expect_identical(AutoCite(bib, "markey"),
 "<a name=cite-markey></a>[[5](http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf)]")
expect_identical(AutoCite(bib, location = "Uppsala", .opts = list(hyperlink = FALSE)),
"[6]")
NoCite(bib = bib, title = "CTAN")
expect_identical(AutoCite(bib, author = "Aristotle"),
"<a name=cite-aristotleanima></a><a name=cite-aristotlephysics></a><a name=cite-aristotlepoetics></a><a name=cite-aristotlerhetoric></a>[[8](#bib-aristotleanima); [9](#bib-aristotlephysics); [10](#bib-aristotlepoetics); [11](#bib-aristotlerhetoric)]")
expect_identical(TextCite(bib, eprinttype = "arxiv"),
 "<a name=cite-baezarticle></a><a name=cite-itzhaki></a><a name=cite-wassenberg></a>[Baez and Lauda [12]](http://arxiv.org/abs/math/0307200v3); [Baez and Lauda [2]](http://arxiv.org/abs/math/0307200v3); [Itzhaki [13]](http://arxiv.org/abs/hep-th/9603067); [Wassenberg and Sanders [14]](http://arxiv.org/abs/1008.2849v1)")
PrintBibliography(bib, .opts = list(check.entries = FALSE))

# an error occurs when a switch is made to numeric citation
BibOptions(cite.style = "authoryear", style = "text", check.entries = FALSE)
Cite(bib, 12)
BibOptions(cite.style = "numeric")
expect_false(grepl("NA", Cite(bib, 12)))
expect_true(grepl("\\[KU\\]", Citet(bib, author = "Kant")))
expect_true(grepl("\\[2\\]", Citep(bib, 14)))
expect_true(grepl("\\[1\\]", Citet(bib, 12)))

# error occurs if cite ref in one style and cite.style then switch *both*
# rm(list = ls(all=TRUE))
unloadNamespace("RefManageR")
library(RefManageR)
#BibOptions(check.entries = FALSE, style = "markdown", bib.style = "numeric", cite.style = "numeric")
BibOptions(check.entries = FALSE) #, cite.style = "alphabetic")
bib <- ReadBib(system.file("Bib", "biblatexExamples.bib",
                           package = "RefManageR"), check = FALSE)
AutoCite(bib, author = "kant")
#BibOptions(cite.style = "authoryear", check.entries = FALSE)
#Cite(bib, 12)
BibOptions(cite.style = "numeric")
expect_false(grepl("NA", Cite(bib, 12)))
expect_true(grepl("\\[KU\\]", Citet(bib, author = "Kant")))
################## end bug

bib <- ReadBib(system.file("Bib", "RJC.bib", package = "RefManageR"))[seq_len(20)]
AutoCite(bib, title = "binary longitudinal data")
expect_true(grepl("[1]", PrintBibliography(bib, .opts = list(cite.style = "numeric"))))
