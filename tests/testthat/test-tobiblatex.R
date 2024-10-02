bib <- c(BibEntry(bibtype = "article",
         key = "shiotsuki2011kasai",
         title = "葛西賢太著,『現代瞑想論-変性意識がひらく世界-』",
         author = "塩,亮子 and 葛西,賢太",
         journal = "宗教と社会",
         volume = 17,
         pages = "67--69",
         year = 2011,
         publisher = "「宗教と社会」学会"),
         BibEntry(bibtype = "article",
                  key = "hiromitsu2022altered",
                  title = "意識状態の変容と脳内ネットワーク",
                  author = "弘光健太郎 and ヒロミツケンタロウ",
                  journal = "鶴見大学仏教文化研究所紀要",
                  volume = 27,
                  pages = "53--66",
                  year = 2022,
                  publisher = "鶴見大学"))

test_that("toBiblatex doesn't replace CJK character name lists with ???? (#106)",
{
  out <- toBiblatex(bib)
  expect_false(any(grepl("[?]", out)))
})

test_that("toBibtex doesn't replace CJK character name lists with ???? (#106)",
{
  out <- toBibtex(bib)
  expect_false(any(grepl("[?]", out)))
})


test_that("toBiblatex preserves correct i accent #102", 
{
  bib.str <- r"[@Article{Ryan-Hamaker-2021,
  author = {Ryan Ois{\'i}n and Ellen L. Hamaker},
  date = {2021-06},
  journaltitle = {Psychometrika},
  title = {Time to intervene: A continuous-time approach to network analysis and centrality},
  doi = {10.1007/s11336-021-09767-0},
  number = {1},
  pages = {214--252},
  volume = {87},
  publisher = {Springer Science and Business Media {LLC}},
}]"
  
  tfile <- tempfile(fileext = ".bib")
  writeLines(bib.str, tfile)
  bib <- ReadBib(tfile)
  out <- toBiblatex(bib)
  expect_true(grepl("\\'{\\i}", out[2], fixed = TRUE))
  out <- toBibtex(bib)
  expect_true(grepl("\\'{\\i}", out[2], fixed = TRUE))
})

bib <- BibEntry(bibtype = "Article", key = "smith2014", title = "An Article Title",
                author = "Smith, Joé", journaltitle = "The Journal Title",
                date = "2014-02-06", pubstate = "forthcoming")
test_that("toBiblatex can toggle encoding names to LaTeX (#105)",
{
  out <- toBiblatex(bib, encoded.names.to.latex = TRUE)
  out.author = out[3]
  expect_true(grepl("\\'{e}", out.author, fixed = TRUE))
  out <- toBiblatex(bib, encoded.names.to.latex = FALSE)
  out.author = out[3]
  expect_false(grepl("\\'{e}", out.author, fixed = TRUE))
})

test_that("toBibtex can toggle encoding names to LaTeX (#105)",
{
  out <- toBibtex(bib, encoded.names.to.latex = TRUE)
  out.author = out[3]
  expect_true(grepl("\\'{e}", out.author, fixed = TRUE))
  out <- toBibtex(bib, encoded.names.to.latex = FALSE)
  out.author = out[3]
  expect_false(grepl("\\'{e}", out.author, fixed = TRUE))
})

bib <- BibEntry(bibtype = "Article", key = "smith2014", title = "An Article Title",
                author = "{NVIDIA Corporation}", journaltitle = "The Journal Title",
                date = "2014-02-06", pubstate = "forthcoming")
test_that("toBiblatex doesn't escape braces in name fields (#109)",
{
  out <- toBiblatex(bib)
  out.author = out[3]
  expect_true(grepl("{{N", out.author, fixed = TRUE))
  out <- toBibtex(bib)
  out.author = out[3]
  expect_true(grepl("{{N", out.author, fixed = TRUE))
})
