context("Reading PDFs")

tmpdir <- tempdir()
tmpfile <- tempfile(fileext = ".zip", tmpdir = tmpdir)
## poppler.f <- download.file("http://dl.dropbox.com/u/3291828/Poppler/poppler.0.22.0_win32.zip",
##                            tmpfile, quiet = TRUE)
## poppler.f <- download.file("http://poppler.freedesktop.org/poppler-0.32.0.tar.xz", tmpfile, quiet = TRUE)
poppler.f <- download.file(
    "ftp://ftp.gnome.org/Public/GNOME/binaries/win32/dependencies/poppler_0.12.0-1_win32.zip", tmpfile,
     quiet = TRUE)
unzip(tmpfile, exdir = tmpdir)
#curdir <- getwd()
exe.path <- file.path(tmpdir, "bin", fsep = "\\")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), exe.path, sep = ":"))
jss.f <- download.file("http://www.jstatsoft.org/v56/i11/paper", paste0(exe.path, "\\jss.pdf"),
                mode = "wb")
arxiv1.f <- download.file("http://arxiv.org/pdf/math/0703791",
              destfile = paste0(exe.path, "\\FIZaop.pdf"), mode = "wb")
arxiv2.f <- download.file("http://arxiv.org/pdf/math/0703858",
              destfile = paste0(exe.path, "\\PBHTaos.pdf"), mode = "wb")
biomet.f <- download.file("http://biomet.oxfordjournals.org/content/83/4/715.full.pdf",
                          destfile = paste0(exe.path, "\\ADVb.pdf"), mode = "wb")

jstor.f <- !file.copy(system.file("pdf", "jstor.pdf", package = "RefManageR"), exe.path)
## jstor.f <- if (capabilities("libcurl"))
##                download.file("https://dl.dropboxusercontent.com/u/107936614/25050155.pdf",  # 25645718.pdf
## #                             destfile = "jstor.pdf", mode = "wb", method = "libcurl")
##            else
##                TRUE

test_that("Recognizes DOI", {
    if (poppler.f || arxiv1.f)
        skip("Couldn't download arxiv1")
    expect_message(ReadPDFs(paste0(exe.path, "\\FIZaop.pdf"), use.metadata = FALSE),
                   "Getting 1 BibTeX entries from CrossRef...")
})

test_that("Creates a BibEntry object", {
    if (poppler.f || all(jstor.f, biomet.f, arxiv1.f, arxiv2.f, jss.f))
        skip("Couldn't download Poppler or a single PDF")
    bib <- ReadPDFs(exe.path, progress = TRUE)
    expect_is(bib, "BibEntry")
})

test_that("Add file field", {
    if (poppler.f || all(jstor.f, biomet.f, arxiv1.f, arxiv2.f, jss.f))
        skip("Couldn't download Poppler or a single PDF")
    expect_is(unlist(bib$file), "character")
})

test_that("Reading one PDF file name", {
    if (poppler.f || jstor.f)
        skip("Couldn't copy jstor.pdf")
    bib1 <- ReadPDFs(paste0(exe.path, "\\jstor.pdf"), use.metadata = FALSE)
    expect_is(bib1, "BibEntry")
    expect_equal(length(bib1), 1L)
})

test_that("Recognizes JSTOR", {
    if (poppler.f || jstor.f)
        skip("Couldn't download Poppler or JSTOR PDF")
    expect_equal(bib[author = "carrol"]$eprinttype, "jstor")
    expect_equal(bib[author = "carrol"]$url, "http://www.jstor.org/stable/25050155")
})

test_that("Recognizes arxiv", {
    if (poppler.f || arxiv2.f)
        skip("Couldn't download arxiv2 reference")
    expect_equal(bib[author = "paul"]$eprinttype, "arxiv")
    expect_equal(bib[author = "paul"]$url, "http://arxiv.org/abs/math/0703858v1")
})

test_that("Can parse metadata date", {
    if (poppler.f || arxiv2.f)
        skip("Couldn't download arxiv2 reference")
    ## Entry misses year/date if use.metadata is FALSE
    expect_equal(bib[author = "paul"]$date, "2013-04-16")
})


test_that("Reading year and author", {
    if (poppler.f || jss.f)
        skip("Couldn't download JSS reference")
    expect_equal(unlist(bib[author = "luo"]$author$family), c("Luo", "Chen", "Su", "Chu"))
    expect_equal(bib[author = "luo"]$year, "2014")
})

test_that("Reading journal and title", {
    if (poppler.f || biomet.f)
        skip("Couldn't download Biometrika reference")
    expect_equal(bib[year = "1996"]$journal, "Biometrika")
    expect_equal(bib[year = "1996"]$title, "The Multivariate Skew-normal Distribution")
    expect_equal(bib[year = "1996"]$bibtype, "Article")
})

test_that("use.metadata = FALSE", {
    bib <- ReadPDFs(exe.path, use.metadata = FALSE, use.crossref = FALSE)
    expect_is(bib, "BibEntry")
    bib <- ReadPDFs(exe.path, use.metadata = FALSE, use.crossref = TRUE)
    expect_is(bib, "BibEntry")
})

# setwd(curdir)
unlink(tmpdir, force = TRUE)
