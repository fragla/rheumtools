test_that("SDAI gives correct answer", {
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=5,phgh=6,crp=7, crp_unit = "mg/dL"), 27)
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=5,phgh=6,crp=7, crp_unit = "mg/L"), 21)
  expect_equal(sdai_score(tjc=4:5,sjc=5:6,ptgh=5:6,phgh=6:7,crp=7:8, crp_unit = "mg/dL"), c(27,32))
  expect_equal(sdai_score(tjc=NA,sjc=5,ptgh=6,phgh=6,crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=4,sjc=NA,ptgh=6,phgh=6,crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=NA,phgh=6,crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=6,phgh=NA,crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=6,phgh="A",crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=30,sjc=5,ptgh=6,phgh=5,crp=7, crp_unit = "mg/dL"), NA_real_)
  expect_equal(sdai_score(tjc=4,sjc=5,ptgh=6,phgh=5,crp=-1, crp_unit = "mg/dL"), NA_real_)

})

test_that("SDAI throws error for incorrect parameters", {
  expect_error(sdai_score(tjc=66,sjc=5,ptgh=5,phgh=6,crp=7, crp_unit = "mg/L", ignore = FALSE))
  expect_error(sdai_score(tjc=26,sjc=32,ptgh=5,phgh=6,crp=7, crp_unit = "mg/L", ignore = FALSE))
  expect_error(sdai_score(tjc=26,sjc=26,ptgh=5,phgh=12,crp=7, crp_unit = "mg/L", ignore = FALSE))
  expect_error(sdai_score(tjc=26,sjc=26,ptgh=-5,phgh=10,crp=7, crp_unit = "mg/L", ignore = FALSE))
  expect_error(sdai_score(tjc=26,sjc=26,ptgh=5,phgh=10,crp=-2, crp_unit = "mg/L", ignore = FALSE))
  expect_error(sdai_score(tjc=4:5,sjc=5:6,ptgh=5:6,phgh=6:7,crp=7, crp_unit = "mg/L"))
})

test_that("SDAI classification gives correct answer", {
  expect_equal(sdai_classification(2), "Remission")
  expect_equal(sdai_classification(8), "Low")
  expect_equal(sdai_classification(18), "Moderate")
  expect_equal(sdai_classification(42), "High")
  expect_equal(sdai_classification(c(2, 13)), c("Remission", "Moderate"))
  expect_equal(sdai_classification(c(27, "A")), c("High", NA_character_))
})

test_that("SDAI classification throws error for incorrect parameters", {
  expect_error(sdai_classification("A", ignore = FALSE))
  expect_error(sdai_classification(-4, ignore = FALSE))
  expect_error(sdai_classification(NA, ignore = FALSE))
})
