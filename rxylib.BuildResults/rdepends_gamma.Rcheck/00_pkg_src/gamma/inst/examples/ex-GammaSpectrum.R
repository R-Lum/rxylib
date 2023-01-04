## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
(spc <- read(spc_file))

## Access
get_hash(spc)
get_names(spc)
get_livetime(spc)
get_realtime(spc)

length(spc)
range_energy(spc)

## Subset
spc[["date"]]
spc[["instrument"]]
spc[["file_format"]]
