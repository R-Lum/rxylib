# SIGNAL STABILIZATION
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname stabilize
#' @aliases signal_stabilize,GammaSpectrum-method
setMethod(
  f = "signal_stabilize",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, f, ...) {
    count <- f(object[["count"]], ...)
    methods::initialize(object, count = count)
  }
)

#' @export
#' @rdname stabilize
#' @aliases signal_stabilize,GammaSpectra-method
setMethod(
  f = "signal_stabilize",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, f, ...) {
    spc <- lapply(X = object, FUN = signal_stabilize, f = f)
    .GammaSpectra(spc)
  }
)
