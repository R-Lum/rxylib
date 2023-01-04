# SIGNAL SPLITING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname signal_split
#' @aliases signal_split,GammaSpectrum-method
setMethod(
  f = "signal_split",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, groups) {

    idx <- split(seq_len(length(object)), f = groups, drop = TRUE)

    z <- lapply(
      X = idx,
      FUN = function(x, spc) {
        signal_slice(spc, x)
      },
      spc = object
    )

    tmp <- .GammaSpectra(z)
    set_names(tmp) <- paste(get_names(object), names(idx), sep = "_")
    tmp
  }
)
