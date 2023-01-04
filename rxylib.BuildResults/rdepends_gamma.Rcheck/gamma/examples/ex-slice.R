## Import CNF files
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Plot spectrum
plot(spc)

## Slice
sliced <- signal_slice(spc)
plot(sliced)

sliced <- signal_slice(spc, -c(1:35))
plot(sliced)

sliced <- signal_slice(sliced, 450:550)
plot(sliced)

## Split
g <- rep(c("A", "B", "C"), c(250, 500, 274))
splited <- signal_split(spc, g)
plot(splited, facet = TRUE)
