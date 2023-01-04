# SIGNAL SMOOTHING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname smooth
#' @aliases signal_smooth,GammaSpectrum-method
setMethod(
  f = "signal_smooth",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("rectangular", "triangular",
                                           "savitzky"), ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    fun <- switch (
      method,
      rectangular = smooth_rectangular,
      triangular = smooth_triangular,
      savitzky = smooth_savitzky,
      stop("There is no such method: ", method, call. = FALSE)
    )

    fun(object, ...)
  }
)

#' @export
#' @rdname smooth
#' @aliases signal_smooth,GammaSpectra-method
setMethod(
  f = "signal_smooth",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, method = c("rectangular", "triangular",
                                           "savitzky"), ...) {
    spc <- lapply(X = object, FUN = signal_smooth, method = method, ...)
    .GammaSpectra(spc)
  }
)
