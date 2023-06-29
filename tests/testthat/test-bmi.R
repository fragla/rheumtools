test_that("BMI gives correct answer", {
  expect_equal(body_mass_index(height = 1.8, weight = 85), 26.2)
  expect_equal(body_mass_index(height = 1.8, weight = NA), NA_real_)
  expect_equal(body_mass_index(height = NA, weight = 85), NA_real_)
  expect_equal(body_mass_index(height = c(1.75, 1.52), weight = c(78, 55.2)), c(25.5, 23.9))
})

test_that("BMI throws error for incorrect parameters", {
  expect_error(body_mass_index(height = 1.8, weight = NA, ignore = FALSE))
  expect_error(body_mass_index(height = NA, weight = 73.2, ignore = FALSE))
  expect_error(body_mass_index(height = 1.75, weight = c(78, 55.2)))
})

test_that("BMI warns for suspected height in centimetres", {
  expect_warning(body_mass_index(height = 180, weight = 75))
})

test_that("BMI classification gives correct answer", {
  expect_equal(body_mass_index_classification(22.9), "Normal")
  expect_equal(body_mass_index_classification("42.5"), "Obese")
  expect_equal(body_mass_index_classification("A"), NA_character_)
  expect_equal(body_mass_index_classification(c(15, 26.2)), c("Underweight", "Overweight"))
})

test_that("BMI classification throws error for incorrect parameters", {
  expect_error(body_mass_index_classification(NA, ignore = FALSE))
  expect_error(body_mass_index_classification("B", ignore = FALSE))
})
