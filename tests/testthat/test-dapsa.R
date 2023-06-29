test_that("DAPSA gives correct answer", {
  expect_equal(dapsa_score(tjc=4,sjc=5,pain=7,ptgh=6,crp=7), 29)
  expect_equal(dapsa_score(tjc=NA,sjc=5,pain=7,ptgh=6,crp=7), NA_real_)
  expect_equal(dapsa_score(tjc=4,sjc=NA,pain=7,ptgh=6,crp=7), NA_real_)
  expect_equal(dapsa_score(tjc=4,sjc=5,pain=NA,ptgh=6,crp=7), NA_real_)
  expect_equal(dapsa_score(tjc=4,sjc=5,pain=7,ptgh=NA,crp=7), NA_real_)
  expect_equal(dapsa_score(tjc=4,sjc=5,pain=7,ptgh=6,crp="High"), NA_real_)
  expect_equal(dapsa_score(tjc=4:5,sjc=5:6,pain=7:8,ptgh=6:7,crp=7:8), c(29, 34))
})

test_that("DAPSA throws error for incorrect parameters", {
  expect_error(dapsa_score(tjc=NA,sjc=5,pain=7,ptgh=6,crp=7, ignore = FALSE))
  expect_error(dapsa_score(tjc=4,sjc="A",pain=7,ptgh=6,crp=7, ignore = FALSE))
  expect_error(dapsa_score(tjc=4,sjc=5,pain="B",ptgh=6,crp=7, ignore = FALSE))
  expect_error(dapsa_score(tjc=4,sjc=5,pain=7,ptgh="C",crp=7, ignore = FALSE))
  expect_error(dapsa_score(tjc=4,sjc=5,pain=7,ptgh=6,crp="D", ignore = FALSE))
  expect_error(dapsa_score(tjc=4:5,sjc=5,pain=7:8,ptgh=6:7,crp=7:8))
})

test_that("Clinical DAPSA gives correct answer", {
  expect_equal(clinical_dapsa_score(tjc=4,sjc=5,pain=7,ptgh=6), 22)
  expect_equal(clinical_dapsa_score(tjc=NA,sjc=5,pain=7,ptgh=6), NA_real_)
  expect_equal(clinical_dapsa_score(tjc=4,sjc=NA,pain=7,ptgh=6), NA_real_)
  expect_equal(clinical_dapsa_score(tjc=4,sjc=5,pain=NA,ptgh=6), NA_real_)
  expect_equal(clinical_dapsa_score(tjc=4,sjc=5,pain=7,ptgh=NA), NA_real_)
  expect_equal(clinical_dapsa_score(tjc=4:5,sjc=5:6,pain=7:8,ptgh=6:7), c(22, 26))
})

test_that("Clinical DAPSA throws error for incorrect parameters", {
  expect_error(clinical_dapsa_score(tjc=NA,sjc=5,pain=7,ptgh=6, ignore = FALSE))
  expect_error(clinical_dapsa_score(tjc=4,sjc="A",pain=7,ptgh=6, ignore = FALSE))
  expect_error(clinical_dapsa_score(tjc=4,sjc=5,pain="B",ptgh=6, ignore = FALSE))
  expect_error(clinical_dapsa_score(tjc=4,sjc=5,pain=7,ptgh="C", ignore = FALSE))
  expect_error(clinical_dapsa_score(tjc=4:5,sjc=5,pain=7:8,ptgh=6:7))
})

test_that("DAPSA classification gives correct answer", {
  expect_equal(dapsa_classification(3), "Remission")
  expect_equal(dapsa_classification(10), "Low")
  expect_equal(dapsa_classification(28), "Moderate")
  expect_equal(dapsa_classification(42), "High")
  expect_equal(dapsa_classification(c(2, 13)), c("Remission", "Low"))
  expect_equal(dapsa_classification(c(24, "A")), c("Moderate", NA_character_))
})

test_that("DAPSA classification throws error for incorrect parameters", {
  expect_error(dapsa_classification("A", ignore = FALSE))
  expect_error(dapsa_classification(-4, ignore = FALSE))
  expect_error(dapsa_classification(NA, ignore = FALSE))
})

test_that("DAPSA percentage response classification gives correct answer", {
  expect_equal(dapsa_response(85.5), "Major")
  expect_equal(dapsa_response(77), "Moderate")
  expect_equal(dapsa_response(51), "Minor")
  expect_equal(dapsa_response(-21), "None")
  expect_equal(dapsa_response(c(88, 10, 53, 79)), c("Major", "None", "Minor", "Moderate"))
  expect_equal(dapsa_response(NA), NA_character_)
  expect_equal(dapsa_response("Major"), NA_character_)
})

test_that("DAPSA percentage response classificationthrows error for incorrect parameters", {
  expect_error(dapsa_response(c(88, NA, 53, 79), ignore = FALSE))
  expect_error(dapsa_response("Moderate", ignore = FALSE))
  expect_error(dapsa_response(NA, ignore = FALSE))
})
