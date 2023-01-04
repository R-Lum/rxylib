# GammaSpectrum ================================================================
test_that("Plot GammaSpectrum", {
  testthat::skip_if_not_installed("vdiffr")
  spc_file_cnf <- system.file("extdata/LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file_cnf)

  gg_spectrum_channel <- plot(spc_cnf, xaxis = "channel")
  vdiffr::expect_doppelganger("spectrum_channel", gg_spectrum_channel)

  gg_spectrum_energy <- plot(spc_cnf, xaxis = "energy")
  vdiffr::expect_doppelganger("spectrum_energy", gg_spectrum_energy)

  gg_spectrum_count <- plot(spc_cnf, yaxis = "count")
  vdiffr::expect_doppelganger("spectrum_count", gg_spectrum_count)

  gg_spectrum_rate <- plot(spc_cnf, yaxis = "rate")
  vdiffr::expect_doppelganger("spectrum_rate", gg_spectrum_rate)

  spc_file_tka <- system.file("extdata/LaBr.TKA", package = "gamma")
  spc_tka <- read(spc_file_tka)

  expect_warning(plot(spc_tka, xaxis = "energy"),
                 "The energy scale is missing, displaying channels instead.")
})
test_that("Plot GammaSpectrum and PeakPosition", {
  testthat::skip_if_not_installed("vdiffr")
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)
  peaks <- peaks_find(spc_cnf)

  gg_peaks <- plot(spc_cnf, peaks)
  vdiffr::expect_doppelganger("spectrum_peaks", gg_peaks)

  peaks@hash <- paste0(rep("x", 32), collapse = "")
  expect_error(plot(spc_cnf, peaks), "`x` and `y` do not match.")
})
# GammaSpectra =================================================================
test_that("Plot GammaSpectra", {
  skip("Weird error with winbuilder devel")
  testthat::skip_if_not_installed("vdiffr")
  spc_dir <- system.file("extdata/", package = "gamma")
  spectra <- read(spc_dir)[-1]

  for (i in c(TRUE, FALSE)) {
    gg_spectra <- plot(spectra, facet = i)
    vdiffr::expect_doppelganger(paste0("spectra_facet-", i), gg_spectra)
  }

  spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  spectrum <- methods::as(read(spc_file), "GammaSpectra")
  expect_warning(plot(spectrum, xaxis = "energy"),
                 "The energy scale is missing for one or more spectra")
})
# Baseline =====================================================================
test_that("Plot Baseline from GammaSpectrum", {
  testthat::skip_if_not_installed("vdiffr")
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- signal_slice(read(spc_file), -(1:40))
  baseline <- signal_baseline(spectrum)

  gg_baseline_count <- plot(baseline, yaxis = "count")
  vdiffr::expect_doppelganger("baseline_count", gg_baseline_count)

  gg_baseline_rate <- plot(baseline, yaxis = "rate")
  vdiffr::expect_doppelganger("baseline_rate", gg_baseline_rate)

  gg_baseline_spc <- plot(spectrum, baseline)
  vdiffr::expect_doppelganger("baseline_spectrum", gg_baseline_spc)

  correct <- signal_correct(spectrum)
  gg_correct_count <- plot(correct, yaxis = "count")
  vdiffr::expect_doppelganger("baseline_correct_count", gg_correct_count)

  gg_correct_rate <- plot(correct, yaxis = "rate")
  vdiffr::expect_doppelganger("baseline_correct_rate", gg_correct_rate)
})
test_that("Plot Baseline from GammaSpectra", {
  testthat::skip_if_not_installed("vdiffr")
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spectra <- signal_slice(read(spc_dir), -c(1:40))
  baseline <- signal_baseline(spectra)

  gg_baselines_count <- plot(baseline, yaxis = "count")
  vdiffr::expect_doppelganger("baselines_count", gg_baselines_count)

  gg_baselines_rate <- plot(baseline, yaxis = "rate")
  vdiffr::expect_doppelganger("baselines_rate", gg_baselines_rate)

  correct <- signal_correct(spectra)
  gg_correct_count <- plot(correct, yaxis = "count")
  vdiffr::expect_doppelganger("baselines_correct_count", gg_correct_count)

  gg_correct_rate <- plot(correct, yaxis = "rate")
  vdiffr::expect_doppelganger("baselines_correct_rate", gg_correct_rate)
})
# CalibrationCurve =============================================================
test_that("Plot CalibrationCurve", {
  testthat::skip_if_not_installed("vdiffr")
  data("BDX_LaBr_1")

  for (i in c(TRUE, FALSE)) {
    gg_calibration <- plot(BDX_LaBr_1, error_ellipse = i)
    vdiffr::expect_doppelganger(paste0("gg_calib_ellipse-", i), gg_calibration)
  }
  for (i in c(TRUE, FALSE)) {
    gg_calibration <- plot(BDX_LaBr_1, error_bar = i)
    vdiffr::expect_doppelganger(paste0("gg_calib_bar-", i), gg_calibration)
  }
  for (i in c(TRUE, FALSE)) {
    gg_calibration <- plot(BDX_LaBr_1, energy = i)
    vdiffr::expect_doppelganger(paste0("gg_calib_energy-", i), gg_calibration)
  }
})
