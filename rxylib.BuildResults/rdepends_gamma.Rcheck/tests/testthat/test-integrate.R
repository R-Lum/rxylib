test_that("Integrate GammaSpectrum", {
  spc_cnf <- system.file("extdata/LaBr.CNF", package = "gamma")
  cnf <- read(spc_cnf)

  spc_bkg <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(spc_bkg)

  int1 <- signal_integrate(cnf, range = c(200, 2800), energy = TRUE)
  expect_equal(int1, c(1.483392e+05, 9.361146e+00),
               tolerance = 1e-06, ignore_attr = TRUE)
  expect_length(int1, 2)

  int2 <- signal_integrate(cnf, bkg, range = c(200, 2800), energy = FALSE)
  expect_equal(int2, c(258.6836050, 0.4129534), tolerance = 1e-07,
               ignore_attr = TRUE)
  expect_length(int2, 2)

  expect_error(signal_integrate(cnf, range = c(200)),
               "must be a numeric vector of length 2")

  spc_tka <- system.file("extdata/LaBr.TKA", package = "gamma")
  tka <- read(spc_tka)
  expect_error(signal_integrate(tka, range = c(200, 2800)),
               "You must calibrate the energy scale of your spectrum first.")
})

test_that("Integrate GammaSpectra", {
  spc_cnf <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  cnf <- read(spc_cnf)

  spc_bkg <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(spc_bkg)

  int1 <- signal_integrate(cnf, range = c(200, 2800), simplify = TRUE)
  expect_type(int1, "double")
  expect_equal(ncol(int1), 2)
  expect_equal(nrow(int1), length(cnf))

  int2 <- signal_integrate(cnf, bkg, range = c(200, 2800), simplify = FALSE)
  expect_type(int2, "list")
  expect_length(int2, length(cnf))
  expect_equal(lengths(int2), rep(2, length(cnf)), ignore_attr = TRUE)
})
