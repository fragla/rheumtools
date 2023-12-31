test_that("Percentage response gives correct answer", {
  expect_equal(percentage_response(12, 3), 75)
  expect_equal(percentage_response(c(12, 18), c(3, 9)), c(75, 50))
  expect_equal(percentage_response(NA, 3), NA_real_)
  expect_equal(percentage_response(12, NA), NA_real_)
  expect_equal(percentage_response(10, -10), NA_real_)
})

test_that("Percentage response throws error for incorrect parameters", {
  expect_error(percentage_response(c(12, 18), c(3, NA), ignore = FALSE))
  expect_error(percentage_response(c(12, 18), 3, ignore = FALSE))
  expect_error(percentage_response(12, "A", ignore = FALSE))
  expect_error(percentage_response(-4, "B", ignore = FALSE))
  expect_error(percentage_response(NA, NA, ignore = FALSE))
})

test_that("is_date gives correct answer", {
  expect_true(is_date(as.Date("2010-01-01")))
  expect_true(is_date(as.Date("12/01/1999", format="%m/%d/%Y")))
  expect_false(is_date("2010-01-01"))
  expect_false(is_date(2010-01-01))
})

test_that("is_date gives correct answer", {
  expect_true(is_date(as.Date("2010-01-01")))
  expect_true(is_date(as.Date("12/01/1999", format="%m/%d/%Y")))
  expect_false(is_date("2010-01-01"))
  expect_false(is_date(2010-01-01))
})

test_that("dates_to_duration gives correct answer", {
  expect_equal(dates_to_duration(as.Date("2020-01-01"), as.Date("12/01/2020", format="%d/%m/%Y")), 11)
  expect_equal(dates_to_duration(as.Date("2020-01-01"), NA), NA)
})

test_that("dates_to_duration throws error for incorrect parameters", {
  expect_error(dates_to_duration(as.Date("2020-01-01"), "12/01/2020"))
  expect_error(dates_to_duration("2019-05-06", as.Date("2020-01-01")))
})
