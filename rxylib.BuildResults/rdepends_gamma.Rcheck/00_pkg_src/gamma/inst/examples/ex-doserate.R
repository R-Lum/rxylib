## Import CNF files
## Spectra
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)

## Background
bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
bkg <- read(bkg_dir)

## Get dose rate values
data("clermont")
(doses <- clermont[, c("gamma_dose", "gamma_error")])

## Build the calibration curve
calib_curve <- dose_fit(spc, bkg, doses,
                        range_Ni = c(300, 2800),
                        range_NiEi = c(165, 2800))

## Plot the curve
plot(calib_curve, threshold = "Ni")

## Estimate gamma dose rates
dose_predict(calib_curve, spc)
