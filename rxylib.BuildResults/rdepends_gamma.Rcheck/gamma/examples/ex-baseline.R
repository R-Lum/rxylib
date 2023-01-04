## Import a CNF file
spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
spc <- read(spc_file)

## Remove the first 35 channels
spc <- signal_slice(spc, -c(1:35))

## Linear baseline
bsl_linear <- baseline_linear(spc, from = 250, to = 750)
plot(spc, bsl_linear)

## SNIP baseline
bsl_snip <- baseline_snip(spc, LLS = FALSE, decreasing = FALSE, n = 100)
plot(spc, bsl_snip)

## Rubberband baseline
bsl_rubber <- baseline_rubberband(spc)
plot(spc, bsl_rubber)

## Remove baseline
spc_clean1 <- signal_correct(spc)
spc_clean2 <- spc - bsl_snip
all(spc_clean1 == spc_clean2)

plot(spc_clean1)
