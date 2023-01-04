# GammaSpectrum ================================================================
test_that("GammaSpectrum to matrix or data.frame", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- read(spc_file)

  mtx <- as.matrix(spectrum)
  expect_type(mtx, "double")
  expect_equal(dim(mtx), c(1024, 4))

  df <- as.data.frame(spectrum)
  expect_type(df, "list")
  expect_equal(dim(df), c(1024, 4))
})
# GammaSpectra =================================================================
test_that("GammaSpectra from list", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spectrum <- list(read(spc_file))

  spectra <- methods::as(spectrum, "GammaSpectra")
  expect_length(spectra, 1)
  expect_equal(names(spectra), "LaBr")
})
