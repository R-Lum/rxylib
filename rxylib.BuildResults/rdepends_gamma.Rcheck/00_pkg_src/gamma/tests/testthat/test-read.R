test_that("Read a TKA file", {
  tka_file <- system.file("extdata/LaBr.TKA", package = "gamma")
  tka_spectrum <- read(tka_file)
  expect_output(show(tka_spectrum), "Gamma spectrum")
})
test_that("Read a CNF file", {
  cnf_file <- system.file("extdata/xxx.CNF", package = "gamma")
  expect_error(read(cnf_file))

  cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  cnf_spectrum <- read(cnf_file)
  expect_output(show(cnf_spectrum), "Gamma spectrum")
})
test_that("Read a set of CNF files", {
  spc_dir <- system.file("extdata/xxx", package = "gamma")
  expect_error(read(spc_dir))

  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spectra <- read(spc_dir)
  expect_output(show(spectra), "A collection of 7 gamma spectra")
})
