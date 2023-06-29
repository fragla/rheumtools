test_that("BASDAI gives correct answer", {
  expect_equal(basdai_score(1, 2, 3, 4, 5, 10), 3.5)
  expect_equal(basdai_score(1:2, 2:3, 3:4, 4:5, 5:6, 6:7), c(3.1, 4.1))
  expect_equal(basdai_score(0, 0, 3, 4, 5, 0), 1.9)
  expect_equal(basdai_score(NA, 2, 3, 4, 5, 10), NA_real_)
  expect_equal(basdai_score(1, 21, 3, 4, 5, 10), NA_real_)
  expect_equal(basdai_score(1, 2, NA, 4, 5, 10), NA_real_)
  expect_equal(basdai_score(1, 2, 3, "B", 5, 10), NA_real_)
  expect_equal(basdai_score(1, 2, 3, 5, NA, 10), NA_real_)
  expect_equal(basdai_score(4, 2, 3, 4, 5, 12), NA_real_)
})

test_that("BASDAI throws error for incorrect parameters", {
  expect_error(basdai_score(11, 2, 3, 4, 5, 10, ignore = FALSE))
  expect_error(basdai_score(1, 21, 3, 4, 5, 10, ignore = FALSE))
  expect_error(basdai_score(1, 2, NA, 4, 5, 10, ignore = FALSE))
  expect_error(basdai_score(1, 2, 3, "B", 5, 10, ignore = FALSE))
  expect_error(basdai_score(1, 2, 3, 4, 52, 10, ignore = FALSE))
  expect_error(basdai_score(1:2, 2, 3, 4, 5, 1, ignore = FALSE))
})

