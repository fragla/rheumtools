test_that("ESR classification gives correct answer", {
  expect_equal(esr_classification(esr = 14, age = 50, sex = "Male"), "Negative")
  expect_equal(esr_classification(esr = 16, age = 50, sex = "Female"), "Negative")
  expect_equal(esr_classification(esr = 15, age = 55, sex = "Male"), "Negative")
  expect_equal(esr_classification(esr = 25, age = 60, sex = "Female"), "Negative")
  expect_equal(esr_classification(esr = 17, age = 50, sex = "Male"), "Positive")
  expect_equal(esr_classification(esr = 25, age = 50, sex = "Female"), "Positive")
  expect_equal(esr_classification(esr = 30, age = 55, sex = "Male"), "Positive")
  expect_equal(esr_classification(esr = 35, age = 60, sex = "Female"), "Positive")
  expect_equal(esr_classification(esr = c(35,30), age = c(60,55), sex = c("Female", "Male")), c("Positive", "Positive"))
  expect_equal(esr_classification(esr = NA, age = 60, sex = "Female"), NA_character_)
  expect_equal(esr_classification(esr = 25, age = NA, sex = "Female"), NA_character_)
  expect_equal(esr_classification(esr = 16, age = 50, sex = NA), NA_character_)
})

test_that("ESR classification throws error for incorrect parameters", {
  expect_error(esr_classification(esr = NA, age = 50, sex = "Male", ignore = FALSE))
  expect_error(esr_classification(esr = 25, age = NA, sex = "Male", ignore = FALSE))
  expect_error(esr_classification(esr = 25, age = 50, sex = "Child", ignore = FALSE))
})

test_that("CRP classification gives correct answer", {
  expect_equal(crp_classification(crp = 0.1), "Normal")
  expect_equal(crp_classification(crp = 0.8), "Normal or minor elevation")
  expect_equal(crp_classification(crp = 5), "Moderate elevation")
  expect_equal(crp_classification(crp = c(11, 55)), c("Marked elevation", "Severe elevation"))
  expect_equal(crp_classification(crp = 11, cutoff = 10), "Positive")
  expect_equal(crp_classification(crp = 0.8, cutoff = 11), "Negative")
  expect_equal(crp_classification(crp = NA), NA_character_)
  expect_equal(crp_classification(crp = NA, cutoff = 11), NA_character_)
})

test_that("CRP classification throws error for incorrect parameters", {
  expect_error(crp_classification(crp = NA, ignore = FALSE))
  expect_error(crp_classification(crp = NA, cutoff = 11, ignore = FALSE))
})
