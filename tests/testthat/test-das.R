test_that("DAS28-ESR gives correct answer", {
  expect_equal(das_28_esr_score(4,5,7,50), 3.81)
  expect_equal(das_28_esr_score(NA,5,7,50), NA_real_)
  expect_equal(das_28_esr_score(4,"A",7,50), NA_real_)
  expect_equal(das_28_esr_score(4,5,"A",50), NA_real_)
  expect_equal(das_28_esr_score(4,5,7,"A"), NA_real_)
  expect_equal(das_28_esr_score(4:5,5:6,7:8,50:51), c(3.81, 4.11))
})

test_that("DAS28-ESR throws error for incorrect parameters", {
  expect_error(das_28_esr_score(NA,5,7,50, ignore = FALSE))
  expect_error(das_28_esr_score(4,NA,7,50, ignore = FALSE))
  expect_error(das_28_esr_score(4,5,NA,50, ignore = FALSE))
  expect_error(das_28_esr_score(4,5,7,NA, ignore = FALSE))
  expect_error(das_28_esr_score(4:5,5:6,7:8,50))
})

test_that("DAS28-CRP gives correct answer", {
  expect_equal(das_28_crp_score(4,5,7,50), 4.15)
  expect_equal(das_28_crp_score(NA,5,7,50), NA_real_)
  expect_equal(das_28_crp_score(4,"A",7,50), NA_real_)
  expect_equal(das_28_crp_score(4,5,"A",50), NA_real_)
  expect_equal(das_28_crp_score(4,5,7,"A"), NA_real_)
  expect_equal(das_28_crp_score(4:5,5:6,7:8,50:51), c(4.15, 4.40))
})

test_that("DAS28-CRP throws error for incorrect parameters", {
  expect_error(das_28_crp_score(NA,5,7,50, ignore = FALSE))
  expect_error(das_28_crp_score(4,NA,7,50, ignore = FALSE))
  expect_error(das_28_crp_score(4,5,NA,50, ignore = FALSE))
  expect_error(das_28_crp_score(4,5,7,NA, ignore = FALSE))
})

test_that("DAS28 disease activity classification gives correct answer", {
  expect_equal(das_28_classification(1.80), "Remission")
  expect_equal(das_28_classification(3.13), "Low")
  expect_equal(das_28_classification(4.25), "Moderate")
  expect_equal(das_28_classification(5.11), "High")
  expect_equal(das_28_classification(c(1.80, 3.13)), c("Remission", "Low"))
  expect_equal(das_28_classification(c(4.25, "A")), c("Moderate", NA))
})

test_that("DAS28 disease activity classification throws error for incorrect parameters", {
  expect_error(das_28_classification(NA, ignore = FALSE))
  expect_error(das_28_classification(c(4.25, "A"), ignore = FALSE))
})

test_that("EULAR response disease activity classification gives correct answer", {
  expect_equal(eular_ra_response(5.21, 1.8), "Good")
  expect_equal(eular_ra_response(4.2, 3.2), "Moderate")
  expect_equal(eular_ra_response(3.1, 2.9), "No response")
  expect_equal(eular_ra_response(4.3, 3.3), "Moderate")
  expect_equal(eular_ra_response(4.0, 3.3), "Moderate")
  expect_equal(eular_ra_response(4.4, 4.2), "No response")
  expect_equal(eular_ra_response(7.46, 6.19), "Moderate")
  expect_equal(eular_ra_response(7.43, 6.81), "No response")
  expect_equal(eular_ra_response(7.19, 6.70), "No response")
  expect_equal(eular_ra_response(NA, 1.8), NA_character_)
  expect_equal(eular_ra_response(5.21, NA), NA_character_)
  expect_equal(eular_ra_response(NA, NA), NA_character_)
  expect_equal(eular_ra_response(c(5.21, 7.19), c(1.80, 6.70)), c("Good", "No response"))
})

test_that("EULAR response disease activity classification throws error for incorrect parameters", {
  expect_error(eular_ra_response(5.21, NA, ignore = FALSE))
  expect_error(eular_ra_response(NA, NA, ignore = FALSE))
  expect_error(eular_ra_response("A", 1.9, ignore = FALSE))
})

