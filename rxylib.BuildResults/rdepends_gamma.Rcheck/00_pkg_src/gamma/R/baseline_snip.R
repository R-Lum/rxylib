# SNIP BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases baseline_snip,GammaSpectrum-method
setMethod(
  f = "baseline_snip",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, LLS = FALSE, decreasing = FALSE, n = 100, ...) {
    # Get counts
    x <- get_counts(object)
    # Estimate baseline
    bsl <- SNIP(x, LLS = LLS, decreasing = decreasing, n = n)
    # Check baseline
    if (anyNA(bsl))
      stop("Failed to estimate the baseline, please check your parameters.",
           call. = FALSE)

    spc <- methods::initialize(object, count = bsl)
    spc <- methods::as(spc, "Baseline")
    set_method(spc) <- "SNIP"
    spc
  }
)

#' @export
#' @rdname baseline
#' @aliases baseline_snip,GammaSpectra-method
setMethod(
  f = "baseline_snip",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, LLS = FALSE, decreasing = FALSE, n = 100, ...) {
    bsl <- lapply(X = object, FUN = baseline_snip,
                  LLS = LLS, decreasing = decreasing, n = n)
    .GammaSpectra(bsl)
  }
)

#' SNIP Algorithm
#'
#' @param x A [`numeric`] vector.
#' @param LLS A [`logical`] scalar: should the LLS operator be applied
#'  on `x` before employing SNIP algorithm?
#' @param decreasing A [`logical`] scalar: should a decreasing clipping window
#'  be used?
#' @param n An [`integer`] value giving the number of iterations.
#' @return A `numeric` vector.
#' @author N. Frerebeau
#' @references
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. and Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  *Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment*,
#'  401(1), p. 113-132. \doi{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. and Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. *Applied Spectroscopy*, 62(1), p. 91-106.
#'  \doi{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. and Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  *Nuclear Instruments and Methods in Physics Research Section B:
#'  Beam Interactions with Materials and Atoms*, 34(3), p. 396-402.
#'  \doi{10.1016/0168-583X(88)90063-8}
#' @keywords internal
#' @noRd
SNIP <- function(x, LLS = FALSE, decreasing = FALSE, n = 100) {
  # Validation
  if (!is.atomic(x) | !is.numeric(x))
    stop("A numeric vector is expected.", call. = FALSE)

  # LLS operator
  x <- if (LLS) LLS(x) else x

  k <- length(x)
  iter <- if (decreasing) rev(seq_len(n)) else seq_len(n)

  tmp <- x
  for (p in iter) {
    for (i in p:(k - p)) {
      a <- x[i]
      b <- (x[i - p] + x[i + p]) / 2
      tmp[i] <- min(a, b)
    }
    x <- tmp
  }

  # Inverse LLS operator
  x <- if (LLS) inverseLLS(x) else x

  return(x)
}

#' @rdname SNIP
#' @keywords internal
#' @noRd
LLS <- function(x) {
  log(log(sqrt(x + 1) + 1) + 1)
}
#' @rdname SNIP
#' @keywords internal
#' @noRd
inverseLLS <- function(x) {
  (exp(exp(x) - 1) - 1)^2 - 1
}
