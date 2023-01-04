## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  comment = "#>",
  out.width = NULL
)

## ----packages-----------------------------------------------------------------
library(gamma)

## ----calib-dose, eval=FALSE---------------------------------------------------
#  # IRAMAT-CRP2A LaBr (BDX1)
#  utils::vignette("CRP2A#1", package = "gamma")
#  
#  # CEREGE NaI (AIX1)
#  utils::vignette("CEREGE#1", package = "gamma")

## ----import-------------------------------------------------------------------
# Automatically skip the first channels
# Import a CNF file
cnf_test <- system.file("extdata/LaBr.CNF", package = "gamma")
(spc_cnf <- read(cnf_test))

# Import a TKA file
tka_test <- system.file("extdata/LaBr.TKA", package = "gamma")
(spc_tka <- read(tka_test))

# Import all files in a given directory
files_test <- system.file("extdata", package = "gamma")
(spc <- read(files_test))

## ----inspect, fig.width=7, fig.height=5, fig.align="center"-------------------
# Plot CNF spectrum
plot(spc_cnf) +
  ggplot2::theme_bw()

## ----slice--------------------------------------------------------------------
# Use a square root transformation
sliced <- signal_slice(spc_tka)

## ----stabilize----------------------------------------------------------------
# Use a square root transformation
transformed <- signal_stabilize(sliced, f = sqrt)

## ----smoothing----------------------------------------------------------------
# Use a 21 point Savitzky-Golay-Filter to smooth the spectrum
smoothed <- signal_smooth(transformed, method = "savitzky", m = 21)

## ----baseline-estimate, fig.width=7, fig.height=5, fig.align="center"---------
# Estimate the baseline of a single file
baseline <- signal_baseline(smoothed, method = "SNIP", decreasing = TRUE)

# Plot spectrum + baseline
plot(smoothed, baseline) +
  ggplot2::labs(title = "Spectrum + baseline") +
  ggplot2::theme_bw()

## ----baseline-remove, fig.width=7, fig.height=5, fig.align="center"-----------
# Substract the estimated baseline
corrected <- smoothed - baseline
# Or, remove the baseline in one go
# corrected <- removeBaseline(smoothed)

# Plot the corrected spectrum
plot(corrected) +
  ggplot2::labs(title = "Baseline-corrected spectrum") +
  ggplot2::theme_bw()

## ----baseline-remove-multi, eval=FALSE----------------------------------------
#  # You can remove the baseline of multiple spectra in one go
#  # Note that the same parameters will be used for all spectra
#  clean <- signal_correct(spc)

## ----peak-detection-----------------------------------------------------------
# Detect peaks in a single file
peaks <- peaks_find(corrected)

# Plot spectrum + peaks
plot(corrected, peaks) +
  ggplot2::labs(title = "Peaks") +
  ggplot2::theme_bw()

## ----calibrate-fit, fig.width=7, fig.height=5, fig.align="center"-------------
# Set the energy values (in keV)
set_energy(peaks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)
peaks

# Calibrate the spectrum using the peak positions
scaled <- energy_calibrate(spc_tka, peaks)

# Plot the spectrum
plot(scaled, xaxis = "energy") +
  ggplot2::labs(title = "Calibrated spectrum") +
  ggplot2::theme_bw()

## ----calibrate-fixed----------------------------------------------------------
# Create a list of channel-enegy pairs
calib_lines <- list(
  channel = c(84, 492, 865),
  energy = c(238, 1461, 2615)
)

# Calibrate the spectrum using these fixed lines
scaled2 <- energy_calibrate(spc_tka, lines = calib_lines)

## ----pipe, fig.width=7, fig.height=5, fig.align="center", fig.show="hold"-----
library(magrittr)

# Spectrum pre-processing and peak detection
pks <- spc_tka %>%
  signal_slice() %>%
  signal_stabilize(f = sqrt) %>%
  signal_smooth(method = "savitzky", m = 21) %>%
  signal_correct(method = "SNIP", decreasing = TRUE, n = 100) %>%
  peaks_find()

# Set the energy values (in keV)
set_energy(pks) <- c(238, NA, NA, NA, 1461, NA, NA, 2615)

# Calibrate the energy scale
cal <- energy_calibrate(spc_tka, pks)

# Plot spectrum
plot(cal, pks) +
  ggplot2::theme_bw()

## ----dose-rate-curves, eval=FALSE---------------------------------------------
#  # Load one of the built-in curves
#  data(BDX_LaBr_1) # IRAMAT-CRP2A (Bordeaux)
#  data(AIX_NaI_1) # CEREGE (Aix-en-Provence)

