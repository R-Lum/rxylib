# Make a fake spectrum with no baseline
cts <- dnorm(1:1024, mean = 86, sd = 5) +
  dnorm(1:1024, mean = 493, sd = 7) +
  dnorm(1:1024, mean = 876, sd = 10)
# Add some noise
set.seed(12345)
cts <- cts * 10^5 + sample(1:10, 1024, TRUE)
spc <- .GammaSpectrum(channel = 1:1024, count = cts)

test_that("Find peaks", {
  peaks <- peaks_find(spc, SNR = 3, span = 50)
  expect_equal(get_channels(peaks), c(86, 493, 876))
  expect_equal(get_energy(peaks, expected = FALSE),
               c(NA_real_, NA_real_, NA_real_))
  expect_equal(get_energy(peaks, expected = TRUE),
               c(NA_real_, NA_real_, NA_real_))

  peaks <- peaks_find(spc, SNR = 3, span = NULL)
  expect_equal(get_channels(peaks), c(86, 493, 876))

  expect_output(show(peaks), "3 peaks were detected")

  # Coerce
  df <- as.data.frame(peaks)
  expect_type(df, "list")
  expect_equal(dim(df), c(3, 3))

  # Getters and setters
  expect_equal(get_hash(peaks), "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
})
test_that("FWHM", {
  df <- methods::as(spc, "data.frame")[, c("channel", "count")]
  fwhm <- FWHM(df, center = 86)
  expect_true(fwhm == 10)
  expect_equal(fwhm / (2 * sqrt(2 * log(2))), 5, tolerance = 0.2)
  expect_identical(FWHM(df, center = 86), FWHM(as.list(df), center = 86))
  expect_error(FWHM(x = 1:5, y = 1:10, center = 86),
               "`x` and `y` lengths differ.")
})
