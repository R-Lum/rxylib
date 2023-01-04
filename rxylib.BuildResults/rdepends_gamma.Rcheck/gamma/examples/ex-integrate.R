## Import CNF files
## Spectra
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)

## Background
bkg_dir <- system.file("extdata/BDX_LaBr_1/background", package = "gamma")
bkg <- read(bkg_dir)

## Integrate background noise spectrum (normalized signal)
(int_bkg <- signal_integrate(bkg, range = c(300, 2800), energy = FALSE))

## Integrate spectra and substract background noise value (net signal)
(int_spc <- signal_integrate(spc, bkg, range = c(300, 2800), energy = FALSE))
