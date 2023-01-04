# ESTIMATE AND REMOVE BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases signal_baseline,GammaSpectrum-method
setMethod(
  f = "signal_baseline",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object,
                        method = c("SNIP", "rubberband", "linear"), ...) {
    # Validation
    method <- match.arg(method, several.ok = FALSE)

    fun <- switch (
      method,
      SNIP = baseline_snip,
      rubberband = baseline_rubberband,
      linear = baseline_linear,
      stop("There is no such method: ", method, call. = FALSE)
    )
    fun(object, ...)
  }
)

#' @export
#' @rdname baseline
#' @aliases signal_baseline,GammaSpectra-method
setMethod(
  f = "signal_baseline",
  signature = signature(object = "GammaSpectra"),
  definition = function(object,
                        method = c("SNIP", "rubberband", "linear"), ...) {
    bsl <- lapply(X = object, FUN = signal_baseline, method = method, ...)
    methods::initialize(object, bsl)
  }
)

#' @export
#' @rdname baseline
#' @aliases signal_correct,GammaSpectrum-method
setMethod(
  f = "signal_correct",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object,
                        method = c("SNIP", "rubberband", "linear"), ...) {
    bsl <- signal_baseline(object, method = method, ...)
    object - bsl
  }
)

#' @export
#' @rdname baseline
#' @aliases signal_correct,GammaSpectra-method
setMethod(
  f = "signal_correct",
  signature = signature(object = "GammaSpectra"),
  definition = function(object,
                        method = c("SNIP", "rubberband", "linear"), ...) {
    bsl <- lapply(X = object, FUN = signal_correct, method = method, ...)
    methods::initialize(object, bsl)
  }
)
