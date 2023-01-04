# Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)
spc <- signal_slice(spc, -c(1:35))

# Plot raw spectrum
spc_clean <- signal_correct(spc)
plot(spc_clean)

# Rectangular smooth
spc_unweighted <- smooth_rectangular(spc, m = 3)
spc_unweighted_clean <- signal_correct(spc_unweighted)
plot(spc_unweighted_clean)

# Triangular smooth
spc_weighted <- smooth_triangular(spc, m = 5)
spc_weighted_clean <- signal_correct(spc_weighted)
plot(spc_weighted_clean)

# Savitzky–Golay
spc_savitzky <- smooth_savitzky(spc, m = 21, p = 2)
spc_savitzky_clean <- signal_correct(spc_savitzky)
plot(spc_savitzky_clean)
