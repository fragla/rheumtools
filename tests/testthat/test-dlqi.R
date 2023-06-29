test_that("DLQI score gives correct answer", {
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "Yes", "Very much", "A lot", "A little"), 17)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "No", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "Very much", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", NA, "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little"), 16)
  expect_equal(dlqi_score(NA, "A lot", "A little","Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", NA, "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", NA, "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", NA, "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", NA, "A lot", "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", NA, "A lot", "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", NA, "Very much", "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", NA, "A lot", "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", NA, "A little"), NA_real_)
  expect_equal(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", NA), NA_real_)
})

test_that("DLQI score throws error for incorrect parameters", {
  expect_error(dlqi_score(NA, "A lot", "A little","Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", NA, "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", NA, "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", NA, "Not relevant", "A lot", "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", NA, "A lot", "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", NA, "A lot", "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", NA, "Very much", "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", NA, "A lot", "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", NA, "A little", ignore = FALSE))
  expect_error(dlqi_score("Very much", "A lot", "A little", "Not at all", "Not relevant", "A lot", "A lot", "Very much", "A lot", NA, ignore = FALSE))
  expect_error(dlqi_score(q1=c("Very much","Very much"), q2=c("A lot","Very much"), q3=c("A little","Very much"), q4=c("Not at all","Very much"), q5=c("Not relevant","Very much"),
                          q6=c("A lot","Very much"), q7="Yes", q8=c("Very much","Very much"), q9=c("A lot","Very much"), q10=c("A little","Very much")))

})

test_that("DLQI classification gives correct answer", {
  expect_equal(dlqi_classification(c(0, 20, 1, 9, 30, 5)), c("no effect at all on patient's life", "very large effect on patient's life", "no effect at all on patient's life", "moderate effect on patient's life", "extremely large effect on patient's life", "small effect on patient's life"))
  expect_equal(dlqi_classification(1), "no effect at all on patient's life")
  expect_equal(dlqi_classification(4), "small effect on patient's life")
  expect_equal(dlqi_classification(10), "moderate effect on patient's life")
  expect_equal(dlqi_classification(15), "very large effect on patient's life")
  expect_equal(dlqi_classification(25), "extremely large effect on patient's life")
  expect_equal(dlqi_classification(NA), NA_character_)
})

test_that("DLQI classification throws error for incorrect parameters", {
  expect_error(dlqi_classification(NA, ignore = FALSE))
  expect_error(dlqi_classification(4.25, ignore = FALSE))
})
