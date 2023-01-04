# GammaSpectrum ================================================================
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spectrum <- read(spc_file)

test_that("Estimate baseline from GammaSpectrum", {
  baseline <- signal_baseline(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["name"]], spectrum[["name"]])
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_equal(baseline[["channel"]], spectrum[["channel"]])
  expect_equal(baseline[["energy"]], spectrum[["energy"]])
  expect_type(baseline[["count"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])
})
test_that("Remove baseline from GammaSpectrum", {
  baseline <- signal_correct(spectrum)

  expect_equal(baseline[["hash"]], spectrum[["hash"]])
  expect_equal(baseline[["name"]], spectrum[["name"]])
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_equal(baseline[["instrument"]], spectrum[["instrument"]])
  expect_equal(baseline[["file_format"]], spectrum[["file_format"]])
  expect_equal(baseline[["live_time"]], spectrum[["live_time"]])
  expect_equal(baseline[["real_time"]], spectrum[["real_time"]])
  expect_equal(baseline[["channel"]], spectrum[["channel"]])
  expect_equal(baseline[["energy"]], spectrum[["energy"]])
  expect_type(baseline[["count"]], "double")
  expect_type(baseline[["rate"]], "double")
  expect_equal(baseline[["calibration"]], spectrum[["calibration"]])
})
# GammaSpectra =================================================================
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spectra <- read(spc_dir)

test_that("Estimate baseline from GammaSpectra", {
  baseline <- signal_baseline(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
})
test_that("Remove baseline from GammaSpectra", {
  baseline <- signal_correct(spectra)

  expect_s4_class(baseline, "GammaSpectra")
  expect_equal(length(baseline), length(spectra))
})
# SNIP =========================================================================
test_that("SNIP algorithm", {
  n <- 6
  baseline <- baseline_snip(spectrum, LLS = TRUE, n = n)
  expect_equal(get_method(baseline), "SNIP")

  spc1 <- spectrum - baseline
  spc2 <- signal_correct(spectrum, method = "SNIP", LLS = TRUE, n = n)

  expect_equal(spc1[["channel"]], spc2[["channel"]])
  expect_equal(spc1[["energy"]], spc2[["energy"]])
  expect_equal(spc1[["count"]], spc2[["count"]])
  expect_equal(spc1[["rate"]], spc2[["rate"]])

  spc3 <- spc1 + baseline
  expect_equal(spc3[["channel"]], spectrum[["channel"]])
  expect_equal(spc3[["energy"]], spectrum[["energy"]])
  expect_equal(spc3[["count"]], spectrum[["count"]])
  expect_equal(spc3[["rate"]], spectrum[["rate"]])

  baseline <- baseline_snip(spectra, LLS = TRUE, n = n)
  expect_equal(length(baseline), length(spectra))

  expect_error(SNIP(LETTERS), "A numeric vector is expected.")
})
# Rubberband ===================================================================
test_that("Rubberband algorithm", {
  noise <- 0
  baseline <- baseline_rubberband(spectrum, noise = noise, spline = TRUE)
  expect_equal(get_method(baseline), "rubberband")

  spc1 <- spectrum - baseline
  spc2 <- signal_correct(spectrum, method = "rubberband",
                         noise = noise, spline = TRUE)

  expect_equal(spc1[["channel"]], spc2[["channel"]])
  expect_equal(spc1[["energy"]], spc2[["energy"]])
  expect_equal(spc1[["count"]], spc2[["count"]])
  expect_equal(spc1[["rate"]], spc2[["rate"]])

  spc3 <- spc1 + baseline
  expect_equal(spc3[["channel"]], spectrum[["channel"]])
  expect_equal(spc3[["energy"]], spectrum[["energy"]])
  expect_equal(spc3[["count"]], spectrum[["count"]])
  expect_equal(spc3[["rate"]], spectrum[["rate"]])

  baseline <- baseline_rubberband(spectra, noise = noise, spline = TRUE)
  expect_equal(length(baseline), length(spectra))

  expect_error(rubberband(LETTERS), "A numeric vector is expected.")
  expect_error(rubberband(1:5, LETTERS), "A numeric vector is expected.")
})
