## Import a Canberra CNF file
cnf_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(cnf_file)
summarise(spc)

## Import all CNF files in a given directory
spc_dir <- system.file("extdata/BDX_LaBr_1/calibration", package = "gamma")
spc <- read(spc_dir)
summarise(spc)
