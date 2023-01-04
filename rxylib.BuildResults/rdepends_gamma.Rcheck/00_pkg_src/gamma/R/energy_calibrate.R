# CALIBRATE ENERGY SCALE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname energy
#' @aliases energy_calibrate,GammaSpectrum,list-method
setMethod(
  f = "energy_calibrate",
  signature = signature(object = "GammaSpectrum", lines = "list"),
  definition = function(object, lines, ...) {
    # Validation
    if (!all(c("channel", "energy") %in% names(lines)))
      stop(sprintf("%s is a list, but does not have components %s and %s.",
                   sQuote("lines"), sQuote("channel"), sQuote("energy")),
           call. = FALSE)

    # Adjust spectrum for energy shift
    # Get corresponding channels
    lines <- data.frame(
      channel = lines$channel,
      energy = lines$energy
    )
    lines <- stats::na.omit(lines)
    n <- nrow(lines)
    if (n < 3) {
      msg <- "You have to provide at least 3 lines for calibration, not %d."
      stop(sprintf(msg, n), call. = FALSE)
    }
    # Get spectrum data
    spc_data <- as.data.frame(object)

    # Adjust spectrum for energy shift
    ## Fit second order polynomial
    fit_poly <- stats::lm(
      formula = energy ~ stats::poly(channel, degree = 2, raw = TRUE),
      data = lines
    )
    ## Predict shifted energy values
    fit_spc <- stats::predict(fit_poly, spc_data[, "channel", drop = FALSE])

    # Return a new gamma spectrum with adjusted energy
    methods::initialize(object, energy = fit_spc, calibration = fit_poly)
  }
)

#' @export
#' @rdname energy
#' @aliases energy_calibrate,GammaSpectrum,PeakPosition-method
setMethod(
  f = "energy_calibrate",
  signature = signature(object = "GammaSpectrum", lines = "PeakPosition"),
  definition = function(object, lines, ...) {
    # Get data
    peaks <- as.data.frame(lines)
    peaks$energy <- peaks$energy_expected

    # Adjust spectrum for energy shift
    # Return a new gamma spectrum with adjusted energy
    energy_calibrate(object, peaks)
  }
)

# Predicates ===================================================================
#' @export
#' @rdname energy
#' @aliases has_energy,GammaSpectrum-method
setMethod(
  f = "has_energy",
  signature = "GammaSpectrum",
  definition = function(object) length(object@energy) != 0
)

#' @export
#' @rdname energy
#' @aliases has_energy,GammaSpectra-method
setMethod(
  f = "has_energy",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = has_energy, FUN.VALUE = logical(1))
  }
)

#' @export
#' @rdname energy
#' @aliases has_calibration,GammaSpectrum-method
setMethod(
  f = "has_calibration",
  signature = "GammaSpectrum",
  definition = function(object) !is.null(object@calibration)
)

#' @export
#' @rdname energy
#' @aliases has_calibration,GammaSpectra-method
setMethod(
  f = "has_calibration",
  signature = "GammaSpectra",
  definition = function(object) {
    vapply(object, FUN = has_calibration, FUN.VALUE = logical(1))
  }
)
