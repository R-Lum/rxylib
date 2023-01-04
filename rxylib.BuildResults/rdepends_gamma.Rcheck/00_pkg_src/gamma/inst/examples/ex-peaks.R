## Import a LaBr spectrum
LaBr_file <- system.file("extdata/LaBr.TKA", package = "gamma")
LaBr_spc <- read(LaBr_file)

## Find peaks by channel
(LaBr_pks <- peaks_find(LaBr_spc)) # Ugly
plot(LaBr_spc, LaBr_pks)

## Search peaks by channel
(LaBr_pks <- peaks_search(LaBr_spc, index = c(86L, 207L, 496L), span = 7))
plot(LaBr_spc, LaBr_pks, split = TRUE)

## Import a BEGe spectrum
BEGe_file <- system.file("extdata/BEGe.CNF", package = "gamma")
BEGe_spc <- read(BEGe_file)

## Search peaks by energy
(BEGe_pks <- peaks_search(BEGe_spc, index = c(47, 63, 911, 1460)))
plot(BEGe_spc, BEGe_pks, split = TRUE)
