file <- system.file("extdata/LaBr.CNF", package = "gamma")
spectrum <- read(file)

# Arith ========================================================================
test_that("Arith", {
  plus <- spectrum + spectrum
  expect_equal(plus@count, spectrum@count * 2)
  expect_equal(plus@rate, spectrum@rate * 2)
  plus <- spectrum + 1
  expect_equal(plus@count, spectrum@count + 1)

  minus <- spectrum - spectrum
  expect_equal(minus@count, rep(0, 1024))

  expect_equal(spectrum * spectrum, spectrum^2)
})
# Compare ======================================================================
test_that("Compare", {
  equal <- spectrum == spectrum
  expect_true(all(equal))

  sup <- spectrum > -1
  expect_true(all(sup))

  inf <- spectrum < 10^6
  expect_true(all(inf))
})
# Logic ========================================================================
test_that("Logic", {
  and <- spectrum & spectrum
  expect_type(and, "logical")
  expect_length(and, 1024)

  and <- spectrum & 0
  expect_false(all(and))

  and <- spectrum | TRUE
  expect_true(all(and))
})
# Math =========================================================================
test_that("Math", {
  expect_equal(cumsum(spectrum), cumsum(spectrum@count))
})
# Math2 ========================================================================
test_that("Math2", {
  expect_equal(signif(spectrum, 2), signif(spectrum@count, 2))
})
# Summary ======================================================================
test_that("Summary", {
  expect_equal(min(spectrum), 0)
  expect_equal(range(spectrum), c(0, 31337))
})
