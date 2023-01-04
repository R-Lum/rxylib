# RUBBERBAND BASELINE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname baseline
#' @aliases baseline_rubberband,GammaSpectrum-method
setMethod(
  f = "baseline_rubberband",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, noise = 0, spline = TRUE, ...) {
    # Get counts
    x <- get_channels(object)
    y <- get_counts(object)
    # Estimate baseline
    bsl <- rubberband(x, y, noise = noise, spline = spline)

    # Check baseline
    if (anyNA(bsl))
      stop("Failed to estimate the baseline, please check your parameters.",
           call. = FALSE)

    spc <- methods::initialize(object, count = bsl)
    spc <- methods::as(spc, "Baseline")
    set_method(spc) <- "rubberband"
    spc
  }
)

#' @export
#' @rdname baseline
#' @aliases baseline_rubberband,GammaSpectra-method
setMethod(
  f = "baseline_rubberband",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, noise = 0, spline = TRUE, ...) {
    bsl <- lapply(X = object, FUN = baseline_rubberband,
                  noise = noise, spline = spline)
    .GammaSpectra(bsl)
  }
)

#' Rubberband Algorithm
#'
#' @param x A [`numeric`] vector.
#' @param y A [`numeric`] vector.
#' @param noise A length-one [`numeric`] vector giving the noise level.
#' @param spline A [`logical`] scalar: should spline interpolation
#'  through the support points be used instead of linear interpolation?
#' @param ... further parameters to be passed to
#'  [stats::predict.smooth.spline()].
#' @details
#'  A convex envelope of the spectrum is determined and the baseline is
#'  estimated as the part of the convex envelope lying below the spectrum.
#' @note
#'  Slightly modified from C. Beleites' [hyperSpec::spc.rubberband()].
#' @keywords internal
#' @noRd
rubberband <- function(x, y, noise = 0, spline = TRUE, ...) {
  # Validation
  if (!is.atomic(x) | !is.numeric(x))
    stop("A numeric vector is expected.", call. = FALSE)
  if (!is.atomic(y) | !is.numeric(y))
    stop("A numeric vector is expected.", call. = FALSE)

  # (chull returns points in clockwise order)
  pts <- grDevices::chull(x, y)

  # Check that ncol(y) is a position 1,
  # if not, rotate chull vertex index so that ncol(y) is at position 1
  # then keep only index from ncol(y) to 1 (i.e. lower part of the hull)
  v_max <- which.max(pts) - 1
  if (v_max > 0) pts <- c(pts[-seq_len(v_max)], pts[seq_len(v_max)])
  pts <- pts[1:which.min(pts)]
  # First and last point must be minima, if not remove them
  if (pts[2] == pts[1] - 1) pts <- pts[-1] # Last point
  pts <- rev(pts) # Sort in ascending order
  if (pts[2] == pts[1] + 1) pts <- pts[-1] # First point

  tmp <- stats::approx(x = x[pts], y = y[pts], xout = x, method = "linear")$y
  if (spline) {
    pts <- which(y <= tmp + noise)
    if (length(pts) > 3) {
      spl <- stats::smooth.spline(x[pts], y[pts], ...)
      tmp <- stats::predict(spl, x, 0)$y
    } else {
      tmp <- stats::spline(x[pts], y[pts], xout = x)$y
    }
  }

  return(tmp)
}
