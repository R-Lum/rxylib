# SNIP BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases baseline_linear,GammaSpectrum-method
setMethod(
  f = "baseline_linear",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, from = NULL, to = NULL) {

    x <- as.data.frame(object)
    from <- if (is.null(from)) 1 else which(x$channel == from[[1L]])
    to <- if (is.null(to)) nrow(x) else which(x$channel == to[[1L]])
    idx <- seq(from = from, to = to, by = 1)

    fit <- stats::lm(count ~ channel, data = x, subset = c(from, to))
    bsl <- stats::predict(fit, x[idx, "channel", drop = FALSE])

    spc_channel <- object@channel[idx]
    spc_energy <- if (has_energy(object)) object@energy[idx] else numeric(0)
    spc <- methods::initialize(object, count = bsl, channel = spc_channel,
                               energy = spc_energy)
    spc <- methods::as(spc, "Baseline")
    set_method(spc) <- "linear"
    spc
  }
)

#' @export
#' @rdname baseline
#' @aliases baseline_linear,GammaSpectra-method
setMethod(
  f = "baseline_linear",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, from = NULL, to = NULL) {
    bsl <- lapply(X = object, FUN = baseline_linear, from = from, to = to)
    .GammaSpectra(bsl)
  }
)
