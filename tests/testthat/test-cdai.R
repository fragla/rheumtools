test_that("CDAI gives correct answer", {
  expect_equal(cdai_score(tjc=4,sjc=5,ptgh=5,phgh=6), 20)
  expect_equal(cdai_score(tjc=NA,sjc=5,ptgh=6,phgh=6), NA_real_)
  expect_equal(cdai_score(tjc=4,sjc=NA,ptgh=6,phgh=6), NA_real_)
  expect_equal(cdai_score(tjc=4,sjc=5,ptgh=NA,phgh=6), NA_real_)
  expect_equal(cdai_score(tjc=4,sjc=5,ptgh=6,phgh=NA), NA_real_)
  expect_equal(cdai_score(tjc=4,sjc=5,ptgh=6,phgh="A"), NA_real_)
  expect_equal(cdai_score(tjc=30,sjc=5,ptgh=6,phgh=5), NA_real_)
})

test_that("CDAI throws error for incorrect parameters", {
  expect_error(cdai_score(tjc=66,sjc=5,ptgh=5,phgh=6, ignore = FALSE))
  expect_error(cdai_score(tjc=26,sjc=32,ptgh=5,phgh=6, ignore = FALSE))
  expect_error(cdai_score(tjc=26,sjc=26,ptgh=5,phgh=12, ignore = FALSE))
  expect_error(cdai_score(tjc=26,sjc=26,ptgh=-5,phgh=10, ignore = FALSE))
})

test_that("CDAI classification gives correct answer", {
  expect_equal(cdai_classification(2), "Remission")
  expect_equal(cdai_classification(8), "Low")
  expect_equal(cdai_classification(18), "Moderate")
  expect_equal(cdai_classification(42), "High")
  expect_equal(cdai_classification(c(2, 13)), c("Remission", "Moderate"))
  expect_equal(cdai_classification(c(24, "A")), c("High", NA_character_))
})

test_that("CDAI classification throws error for incorrect parameters", {
  expect_error(cdai_classification("A", ignore = FALSE))
  expect_error(cdai_classification(-4, ignore = FALSE))
  expect_error(cdai_classification(NA, ignore = FALSE))
})
