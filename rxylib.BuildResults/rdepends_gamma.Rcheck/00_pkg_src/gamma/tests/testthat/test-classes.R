test_that("Initialize an empty GammaSpectrum instance", {
  spectrum <- new("GammaSpectrum")
  expect_output(show(spectrum), "An empty gamma spectrum")

  expect_identical(spectrum[["hash"]], "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  expect_identical(spectrum[["name"]], character(0))
  expect_s3_class(spectrum[["date"]], "POSIXct")
  expect_identical(spectrum[["instrument"]], character(0))
  expect_identical(spectrum[["file_format"]], character(0))
  expect_identical(spectrum[["live_time"]], numeric(0))
  expect_identical(spectrum[["real_time"]], numeric(0))
  expect_identical(spectrum[["channel"]], integer(0))
  expect_identical(spectrum[["energy"]], numeric(0))
  expect_identical(spectrum[["count"]], numeric(0))
  expect_identical(spectrum[["rate"]], numeric(0))
  expect_null(spectrum[["calibration"]])

  expect_error(spectrum[["X"]])
})
test_that("Initialize an empty GammaSpectra instance", {
  expect_s4_class(new("GammaSpectra"), "list")

  spectra <- new("GammaSpectra")
  expect_output(show(spectra), "An empty set of gamma spectra")
})
test_that("Initialize an empty BaseLine instance", {
  baseline <- new("Baseline")
  expect_output(show(baseline), "An empty gamma spectrum")

  expect_identical(baseline[["hash"]], "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  expect_identical(baseline[["name"]], character(0))
  expect_s3_class(baseline[["date"]], "POSIXct")
  expect_identical(baseline[["instrument"]], character(0))
  expect_identical(baseline[["file_format"]], character(0))
  expect_identical(baseline[["live_time"]], numeric(0))
  expect_identical(baseline[["real_time"]], numeric(0))
  expect_identical(baseline[["channel"]], integer(0))
  expect_identical(baseline[["energy"]], numeric(0))
  expect_identical(baseline[["count"]], numeric(0))
  expect_identical(baseline[["rate"]], numeric(0))
  expect_null(baseline[["calibration"]])

  expect_error(baseline[["X"]])
})
test_that("Initialize an empty DoseRateModel instance", {
  dose <- new("DoseRateModel")
  expect_output(show(dose), "<DoseRateModel>")

  expect_identical(dose[["slope"]], numeric(0))
  expect_identical(dose[["intercept"]], numeric(0))
  expect_identical(dose[["covariance"]], numeric(0))
  expect_identical(dose[["df"]], numeric(0))
  expect_identical(dose[["MSWD"]], numeric(0))
  expect_identical(dose[["p_value"]], numeric(0))
  expect_identical(dose[["range"]], numeric(0))
  expect_s4_class(dose[["data"]], "data.frame")
  expect_identical(dose[["background"]], numeric(0))

  expect_error(dose[["X"]])
})
test_that("Initialize an empty CalibrationCurve instance", {
  calib <- new("CalibrationCurve")
  expect_output(show(calib), "Calibration curve")

  expect_s4_class(calib[["Ni"]], "DoseRateModel")
  expect_s4_class(calib[["NiEi"]], "DoseRateModel")
  expect_type(calib[["details"]], "list")

  expect_error(calib[["X"]])
})
test_that("Initialize an empty PeakPosition instance", {
  peak <- new("PeakPosition")
  expect_output(show(peak), "No peaks were detected")

  expect_identical(peak[["hash"]],"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  expect_identical(peak[["noise_method"]], character(0))
  expect_identical(peak[["noise_threshold"]],  numeric(0))
  expect_identical(peak[["window"]], integer(0))
  expect_identical(peak[["channel"]], integer(0))
  expect_identical(peak[["energy_observed"]], numeric(0))
  expect_identical(peak[["energy_expected"]], numeric(0))

  expect_error(peak[["X"]])

  expect_error(.PeakPosition(noise_method = LETTERS),
               "must be a character vector of length 1, not 26")
  expect_error(.PeakPosition(noise_threshold = 1:26),
               "must be a numeric vector of length 1, not 26")
  expect_error(.PeakPosition(noise_threshold = -1),
               "must be a positive number")
  expect_error(.PeakPosition(window = 1:26),
               "must be an integer vector of length 1, not 26")
  expect_error(.PeakPosition(window = as.integer(-1)),
               "Slot `window` must be a strictly positive integer, not -1.")
  expect_error(.PeakPosition(energy_expected = 1:26),
               "Slots `channel` and `energy_expected` must have the same length.")
  expect_error(.PeakPosition(energy_observed = 1:26),
               "Slots `channel` and `energy_observed` must have the same length.")
})
