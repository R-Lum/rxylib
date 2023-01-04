## Import all CNF files in a given directory
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
(spc <- read(spc_dir))

## Access
get_hash(spc)
get_names(spc)
get_livetime(spc)
get_realtime(spc)

lengths(spc)
range_energy(spc)

## Subset
spc[] # All spectra
spc[NULL] # All spectra
spc[1] # The first spectrum
spc[-6] # Delete the sixth spectrum
spc[1:3] # The first three spectra
spc[c(1, 3)] # The first and third spectra
spc["BRIQUE"] # The spectrum named 'BRIQUE'
spc[c("BRIQUE", "C347")] # The spectra named 'BRIQUE' and 'C347'
spc[1:3, "energy"] # The slot 'energy' of the first three spectra
spc[[1]]
spc[["BRIQUE"]]
