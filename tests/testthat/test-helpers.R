test_that("Percentage response gives correct answer", {
  expect_equal(percentage_response(12, 3), 75)
  expect_equal(percentage_response(c(12, 18), c(3, 9)), c(75, 50))
  expect_equal(percentage_response(NA, 3), NA_real_)
  expect_equal(percentage_response(12, NA), NA_real_)
  expect_equal(percentage_response(10, -10), NA_real_)
})

test_that("Percentage response throws error for incorrect parameters", {
  expect_equal(percentage_response(c(12, 18), c(3, NA), ignore = FALSE))
  expect_equal(percentage_response(c(12, 18), 3, ignore = FALSE))
  expect_error(percentage_response(12, "A", ignore = FALSE))
  expect_error(percentage_response(-4, "B", ignore = FALSE))
  expect_error(percentage_response(NA, NA, ignore = FALSE))
})
