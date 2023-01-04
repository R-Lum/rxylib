# Import CNF files
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)

# Plot all spectra
plot(spc, yaxis = "rate", facet = FALSE) +
  ggplot2::theme_bw()

# Plot the spectrum named 'BRIQUE'
plot(spc, xaxis = "energy", yaxis = "count", select = "BRIQUE") +
  ggplot2::theme_bw()

# Plot the first three spectra
plot(spc, xaxis = "channel", yaxis = "rate", select = 1:3, facet = TRUE) +
  ggplot2::theme_bw()
