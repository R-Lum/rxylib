## Import a CNF file
spc_file <- system.file("extdata/LaBr.TKA", package = "gamma")
(spc <- read(spc_file))

## Set peak positions (channel) and expected energy values
calib_lines <- list(
  channel = c(86, 495, 879),
  energy = c(238, 1461, 2615)
)

## Adjust the energy scale
(spc1 <- energy_calibrate(spc, lines = calib_lines))

## Inspect results
plot(spc1, xaxis = "energy", yaxis = "count") +
  ggplot2::geom_vline(xintercept = c(238, 1461, 2615), linetype = 3)
