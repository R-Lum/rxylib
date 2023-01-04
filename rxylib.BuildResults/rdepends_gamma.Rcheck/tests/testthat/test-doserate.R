data("clermont")

test_that("Build a calibration curve", {
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)

  doses <- as.matrix(clermont[, c("gamma_dose", "gamma_error")])

  calib <- dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))

  expect_error(
    dose_fit(spc, bkg, doses, range_Ni = c(300), range_NiEi = c(165, 2800)),
    "must be of length 2"
  )
})
test_that("Estimate dose rates", {
  spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
  spc <- read(spc_dir)
  bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
  bkg <- read(bkg_dir)

  doses <- clermont[, c("gamma_dose", "gamma_error")]

  calib <- dose_fit(spc, bkg, doses,  range_Ni = c(300, 2800),
                    range_NiEi = c(165, 2800))

  # Missing
  dose_rate1 <- dose_predict(calib)
  expect_type(dose_rate1, "list")
  expect_equal(dim(dose_rate1), c(7, 5))
  # GammaSpectrum
  dose_rate2 <- dose_predict(calib, spc[[1]])
  expect_type(dose_rate2, "list")
  expect_equal(dim(dose_rate2), c(1, 5))
  # GammaSpectra
  dose_rate3 <- dose_predict(calib, spc)
  expect_type(dose_rate3, "list")
  expect_equal(dim(dose_rate3), c(7, 5))
})
test_that("Get residuals", {
  data("BDX_LaBr_1")

  Ni <- get_residuals(BDX_LaBr_1[["Ni"]])
  expect_equal(dim(Ni), c(7, 4))
  expect_equal(colnames(Ni), c("names", "fitted", "residuals", "standardized"))

  NiEi <- get_residuals(BDX_LaBr_1[["NiEi"]])
  expect_equal(dim(NiEi), c(7, 4))
})
