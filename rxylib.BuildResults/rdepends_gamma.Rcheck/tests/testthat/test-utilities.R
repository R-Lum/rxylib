test_that("Equality within a vector", {
  expect_true(isEqual(c(1, 1, 1)))
  expect_false(isEqual(c(1, 1, 1.1)))

  expect_error(isEqual(LETTERS), "A numeric vector is expected.")
})
test_that("Positive numbers", {
  expect_true(isPositive(c(1, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_false(isPositive(c(-1, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_true(isPositive(c(0, 1, 1), strict = FALSE, na.rm = TRUE))
  expect_false(isPositive(c(0, 1, 1), strict = TRUE, na.rm = TRUE))
  expect_true(is.na(isPositive(c(NA, 1, 1), strict = FALSE, na.rm = FALSE)))

  expect_error(isPositive(LETTERS), "A numeric vector is expected.")
})
