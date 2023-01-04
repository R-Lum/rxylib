# SIGNAL SLICING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname signal_slice
#' @aliases signal_slice,GammaSpectrum-method
setMethod(
  f = "signal_slice",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, ...) {
    i <- as.integer(c(...))
    if (length(i) == 0) {
      i <- -seq_len(which.max(object[["count"]]))
    }

    if (!all(i > 0) & !all(i < 0)) {
      stop("A vector of strictly positive or negative integers is expected.",
           call. = FALSE)
    }

    idx <- which(object[["channel"]] %in% abs(i)) * sign(i)
    if (length(idx) == 0) return(object)

    channel <- object[["channel"]][idx]
    energy <- object[["energy"]][idx]
    count <- object[["count"]][idx]

    methods::initialize(object, channel = channel, energy = energy,
                        count = count)
  }
)

#' @export
#' @rdname signal_slice
#' @aliases signal_slice,GammaSpectra-method
setMethod(
  f = "signal_slice",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, ...) {
    spc <- lapply(X = object, FUN = signal_slice, ...)
    .GammaSpectra(spc)
  }
)
