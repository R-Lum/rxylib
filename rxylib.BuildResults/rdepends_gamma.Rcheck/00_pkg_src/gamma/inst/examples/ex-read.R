## Import a Canberra CNF file
cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
(cnf_spc <- read(cnf_file))

## Import a TKA file
tka_file <- system.file("extdata/LaBr.TKA", package = "gamma")
(tka_spc <- read(tka_file))

## Import all files in a given directory
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
(spc <- read(spc_dir))
