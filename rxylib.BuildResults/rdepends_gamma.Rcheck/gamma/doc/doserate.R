## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  fig.width = 7,
  fig.height = 3.5,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
library(gamma)
library(magrittr)

## ----import-------------------------------------------------------------------
# Import CNF files for calibration
spc_dir <- system.file("extdata/AIX_NaI_1/calibration", package = "gamma")
(spc <- read(spc_dir))

# Import a CNF file of background measurement
bkg_dir <- system.file("extdata/AIX_NaI_1/background", package = "gamma")
(bkg <- read(bkg_dir))

## ----signal-------------------------------------------------------------------
# Spectrum pre-processing
# Remove baseline for peak detection
bsl <- spc %>%
  signal_slice(-1:-40) %>%
  signal_stabilize(f = sqrt) %>%
  signal_smooth(method = "savitzky", m = 21) %>%
  signal_correct()

## ----calibrate-BRIQUE---------------------------------------------------------
# Peak detection
pks <- peaks_find(bsl[["BRIQUE"]])
# Set energy values
set_energy(pks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)
# Adjust the energy scale
BRIQUE <- energy_calibrate(spc[["BRIQUE"]], pks)

## ----plot-BRIQUE, echo=FALSE--------------------------------------------------
plot(BRIQUE, pks) +
  ggplot2::theme_bw()

## ----calibrate-C341-----------------------------------------------------------
# Spectrum pre-processing and peak detection
pks <- peaks_find(bsl[["C341"]])
# Set energy values
set_energy(pks) <- c(238, NA, NA, NA, 1461, NA, 2615)
# Adjust the energy scale
C341 <- energy_calibrate(spc[["C341"]], pks)

## ----plot-C341, echo=FALSE----------------------------------------------------
plot(C341, pks) +
  ggplot2::theme_bw()

## ----calibrate-C347-----------------------------------------------------------
# Spectrum pre-processing and peak detection
pks <- peaks_find(bsl[["C347"]], span = 10)
# Set energy values
set_energy(pks) <- c(238, NA, NA, NA, NA, 1461, NA, 2615)
# Adjust the energy scale
C347 <- energy_calibrate(spc[["C347"]], pks)

## ----plot-C347, echo=FALSE----------------------------------------------------
plot(C347, pks) +
  ggplot2::theme_bw()

## ----calibrate-GOU------------------------------------------------------------
# Spectrum pre-processing and peak detection
pks <- peaks_find(bsl[["GOU"]])
# Set energy values
set_energy(pks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)
# Adjust the energy scale
GOU <- energy_calibrate(spc[["GOU"]], pks)

## ----plot-GOU, echo=FALSE-----------------------------------------------------
plot(GOU, pks) +
  ggplot2::theme_bw()

## ----calibrate-PEP------------------------------------------------------------
# Spectrum pre-processing and peak detection
pks <- peaks_find(bsl[["PEP"]])
# Set energy values
set_energy(pks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)
# Adjust the energy scale
PEP <- energy_calibrate(spc[["PEP"]], pks)

## ----plot-PEP, echo=FALSE-----------------------------------------------------
plot(PEP, pks) +
  ggplot2::theme_bw()

## ----calibrate-bkg------------------------------------------------------------
# Pb212, K40, Tl208
lines <- data.frame(
  channel = c(86, 496, 870),
  energy = c(238, 1461, 2615)
)
bkg_scaled <- energy_calibrate(bkg, lines = lines)

## ----plot-bkg, echo=FALSE-----------------------------------------------------
plot(bkg_scaled, xaxis = "energy", yaxis = "rate") +
  ggplot2::geom_vline(xintercept = c(238, 1461, 2615), linetype = 3) +
  ggplot2::theme_bw()

## ----calibrate-spc------------------------------------------------------------
spc_scaled <- list(BRIQUE, C341, C347, GOU, PEP)
spc_scaled <- methods::as(spc_scaled, "GammaSpectra")

## ----integrate-Ni-------------------------------------------------------------
# Integration range (in keV)
Ni_range <- c(200, 2800)

# Integrate background spectrum
(Ni_bkg <- signal_integrate(bkg_scaled, range = Ni_range, energy = FALSE))

# Integrate reference spectra
(Ni_spc <- signal_integrate(spc_scaled, range = Ni_range, background = Ni_bkg, 
                            energy = FALSE, simplify = TRUE))

## ----integrate-NiEi-----------------------------------------------------------
# Integration range (in keV)
NiEi_range <- c(200, 2800)

# Integrate background spectrum
(NiEi_bkg <- signal_integrate(bkg_scaled, range = NiEi_range, energy = TRUE))

# Integrate reference spectra
(NiEi_signal <- signal_integrate(spc_scaled, range = NiEi_range, 
                                 background = NiEi_bkg, energy = TRUE,
                                 simplify = TRUE))

## ----calibration, fig.width=3.5, fig.show='hold'------------------------------
# Get reference dose rates
data("clermont")
doses <- clermont[, c("gamma_dose", "gamma_error")]

# Metadata
info <- list(
  laboratory = "CEREGE",
  instrument = "InSpector 1000",
  detector = "NaI",
  authors = "CEREGE Luminescence Team"
)

# Build the calibration curve
AIX_NaI <- dose_fit(
  spc_scaled, background = bkg_scaled, doses = doses,
  range_Ni = Ni_range, range_NiEi = NiEi_range,
  details = info
)

# Summary
summarise(AIX_NaI)

# Plot curve
plot(AIX_NaI, model = "Ni") +
  ggplot2::theme_bw()
plot(AIX_NaI, model = "NiEi") +
  ggplot2::theme_bw()

## ----save, eval=FALSE, echo=FALSE---------------------------------------------
#  # DANGER ZONE
#  # AIX_NaI_1 <- AIX_NaI
#  # usethis::use_data(AIX_NaI_1, internal = FALSE, overwrite = FALSE)

## ----check-Ni, echo=FALSE, fig.width=3.5, fig.show='hold'---------------------
Ni_residuals  <- get_residuals(AIX_NaI[["Ni"]])

# Residuals vs fitted values
ggplot2::ggplot(Ni_residuals, ggplot2::aes(x = fitted, y = residuals)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 3) +
  ggplot2::geom_segment(ggplot2::aes(x = fitted, xend = fitted,
                                     y = 0, yend = residuals)) + 
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Residuals vs fitted values",
                x = "Fitted values", y = "Residuals")

# Std. residuals vs fitted values
ggplot2::ggplot(Ni_residuals, ggplot2::aes(x = fitted, y = standardized)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 3) +
  ggplot2::geom_hline(yintercept = c(-2, 2), linetype = 2) +
  ggplot2::geom_segment(ggplot2::aes(x = fitted, xend = fitted,
                                     y = 0, yend = standardized)) + 
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Std. residuals vs fitted values",
                x = "Fitted values", y = "Standardized residuals")

# Normal QQ plot of standardized residuals
ggplot2::ggplot(Ni_residuals, ggplot2::aes(sample = standardized)) +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::geom_qq_line(colour = "red") +
  ggplot2::geom_qq() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Normal QQ plot",
                x = "Theoretical quantiles",
                y = "Standardize residuals")

# Cook's distance
# ggplot2::ggplot(Ni_residuals, ggplot2::aes(x = name, y = cook)) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 3) +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::geom_segment(ggplot2::aes(x = name, xend = name,
#                                      y = 0, yend = cook)) +
#   ggplot2::geom_point() +
#   ggplot2::theme_bw() +
#   ggplot2::labs(title = "Cook's distance",
#                 x = "Observation", y = "D")

## ----check-NiEi, echo=FALSE, fig.width=3.5, fig.show='hold'-------------------
NiEi_residuals  <- get_residuals(AIX_NaI[["NiEi"]])

# Residuals vs fitted values
ggplot2::ggplot(NiEi_residuals, ggplot2::aes(x = fitted, y = residuals)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 3) +
  ggplot2::geom_segment(ggplot2::aes(x = fitted, xend = fitted,
                                     y = 0, yend = residuals)) + 
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Residuals vs fitted values",
                x = "Fitted values", y = "Residuals")

# Std. residuals vs fitted values
ggplot2::ggplot(NiEi_residuals, ggplot2::aes(x = fitted, y = standardized)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 3) +
  ggplot2::geom_hline(yintercept = c(-2, 2), linetype = 2) +
  ggplot2::geom_segment(ggplot2::aes(x = fitted, xend = fitted,
                                     y = 0, yend = standardized)) + 
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Std. residuals vs fitted values",
                x = "Fitted values", y = "Standardized residuals")

# Normal QQ plot of standardized residuals
ggplot2::ggplot(NiEi_residuals, ggplot2::aes(sample = standardized)) +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::geom_qq_line(colour = "red") +
  ggplot2::geom_qq() +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Normal QQ plot",
                x = "Theoretical quantiles",
                y = "Standardize residuals")

# Cook's distance
# ggplot2::ggplot(NiEi_residuals, ggplot2::aes(x = name, y = cook)) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 3) +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::geom_segment(ggplot2::aes(x = name, xend = name,
#                                      y = 0, yend = cook)) +
#   ggplot2::geom_point() +
#   ggplot2::theme_bw() +
#   ggplot2::labs(title = "Cook's distance",
#                 x = "Observation", y = "D")

## ----predict------------------------------------------------------------------
# Import CNF files for dose rate prediction
test_dir <- system.file("extdata/AIX_NaI_1/test", package = "gamma")
(test <- read(test_dir))

# Inspect spectra
plot(test, yaxis = "rate") +
  ggplot2::theme_bw()

# Dose rate prediction
# (assuming that the energy scale of each spectrum was adjusted first)
(rates <- dose_predict(AIX_NaI, test, sigma = 2))

## ----session-info, echo=FALSE-------------------------------------------------
sessionInfo()

