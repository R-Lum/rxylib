test_that("Calibrate a GammaSpectrum object with a list", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(spc_file)

  lines <- list(
    channel = c(76, 459, 816),
    energy = c(238, 1461, 2614.5)
  )

  calib <- energy_calibrate(spectrum, lines = lines)
  expect_s4_class(calib, "GammaSpectrum")

  expect_equal(calib[["hash"]], spectrum[["hash"]])
  expect_equal(calib[["name"]], spectrum[["name"]])
  expect_s3_class(calib[["date"]], "POSIXct")
  expect_equal(calib[["instrument"]], spectrum[["instrument"]])
  expect_equal(calib[["file_format"]], spectrum[["file_format"]])
  expect_equal(calib[["live_time"]], spectrum[["live_time"]])
  expect_equal(calib[["real_time"]], spectrum[["real_time"]])
  expect_equal(calib[["channel"]], spectrum[["channel"]])
  expect_length(calib[["energy"]], 1024)
  expect_true(all(calib[["energy"]] != spectrum[["energy"]]))
  expect_equal(calib[["count"]], spectrum[["count"]])
  expect_equal(calib[["rate"]], spectrum[["rate"]])
  expect_s3_class(calib[["calibration"]], "lm")

  lines <- list(
    X = c(76, 459, 816),
    Y = c(238, 1461, 2614.5)
  )
  expect_error(energy_calibrate(spectrum, lines = lines),
               "does not have components")

  lines <- list(
    channel = c(76, 816),
    energy = c(238, 2614.5)
  )
  expect_error(energy_calibrate(spectrum, lines = lines),
               "You have to provide at least 3 lines for calibration, not 2.")
})
test_that("Calibrate a GammaSpectrum object with a PeakPosition object", {
  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum <- read(spc_file)

  peaks <- .PeakPosition(
    hash = spectrum@hash,
    channel = c(76L, 459L, 816L),
    energy_expected = c(NA_real_, NA_real_, NA_real_)
  )

  expect_error(energy_calibrate(spectrum, lines = peaks),
               "You have to provide at least 3 lines for calibration, not 0.")

  set_energy(peaks) <- c(238, 1461, 2614.5)
  calib <- energy_calibrate(spectrum, lines = peaks)

  expect_s4_class(calib, "GammaSpectrum")
  expect_length(spectrum@energy, 0)
  expect_length(calib@energy, 1024)
})
test_that("the energy scale of a GammaSpectrum is set", {
  cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  cnf_spc <- read(cnf_file)
  expect_true(has_energy(cnf_spc))

  tka_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  tka_spc <- read(tka_file)
  expect_false(has_energy(tka_spc))

  set_file <- system.file("extdata/", package = "gamma")
  set_spc <- read(set_file)
  expect_equal(has_energy(set_spc), c(TRUE, TRUE, FALSE), ignore_attr = TRUE)
})

