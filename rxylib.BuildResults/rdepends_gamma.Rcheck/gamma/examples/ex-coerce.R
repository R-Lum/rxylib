## Import a Canberra CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Coerce
mtx <- as.matrix(spc)
df <- as.data.frame(spc)
head(df)
