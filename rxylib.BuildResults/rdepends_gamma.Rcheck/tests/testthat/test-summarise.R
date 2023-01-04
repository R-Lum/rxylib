# GammaSpectrum ================================================================
test_that("GammaSpectrum", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(spc_file)

  s <- summarise(spectrum)
  expect_type(s, "list")
  expect_equal(dim(s), c(1, 7))
})
# GammaSpectra =================================================================
test_that("GammaSpectra", {
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spectra <- read(spc_dir)

  s <- summarise(spectra)
  expect_type(s, "list")
  expect_equal(dim(s), c(7, 7))
})
# CalibrationCurve =============================================================
test_that("CalibrationCurve", {
  data("BDX_LaBr_1")
  expect_type(summarise(BDX_LaBr_1), "list")
})
