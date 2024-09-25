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
