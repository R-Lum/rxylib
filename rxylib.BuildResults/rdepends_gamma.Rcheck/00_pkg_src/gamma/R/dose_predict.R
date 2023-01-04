# PREDICT DOSE RATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,missing-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "missing"),
  definition = function(object, sigma = 1, epsilon = 0) {

    Ni <- predict_york(object[["Ni"]],
                       energy = FALSE, sigma = sigma, epsilon = epsilon)

    NiEi <- predict_york(object[["NiEi"]],
                         energy = TRUE, sigma = sigma, epsilon = epsilon)

    merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,GammaSpectrum-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectrum"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0) {
    spectrum <- methods::as(spectrum, "GammaSpectra")
    dose_predict(object, spectrum, sigma = sigma, epsilon = epsilon)
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_predict,CalibrationCurve,GammaSpectra-method
setMethod(
  f = "dose_predict",
  signature = signature(object = "CalibrationCurve", spectrum = "GammaSpectra"),
  definition = function(object, spectrum, sigma = 1, epsilon = 0) {

    Ni <- predict_york(object[["Ni"]], spectrum,
                       energy = FALSE, sigma = sigma, epsilon = epsilon)

    NiEi <- predict_york(object[["NiEi"]], spectrum,
                         energy = TRUE, sigma = sigma, epsilon = epsilon)

    merge(Ni, NiEi, by = "names", sort = FALSE, suffixes = c("_Ni","_NiEi"))
  }
)

#' @param model A [DoseRateModel-class] object.
#' @param sigma A [`numeric`].
#' @param epsilon A [`numeric`].
#' @return A [`data.frame`].
#' @keywords internal
#' @noRd
predict_york <- function(model, spectrum, energy = FALSE,
                         sigma = 1, epsilon = 0) {
  # Get integration range
  range <- model[["range"]]
  # Get background
  background <- model[["background"]]
  # Integrate spectrum
  if (missing(spectrum)) {
    signals <- model[["data"]]
    signals$value <- signals$signal_value
    signals$error <- signals$signal_error
  } else {
    signals <- signal_integrate(spectrum, background, range = range,
                                energy = energy, simplify = TRUE)
    signals <- as.data.frame(signals)
    signals$names <- rownames(signals)
  }

  # Get linear regression results
  slope <- model[["slope"]]
  intercept <- model[["intercept"]]

  gamma_dose <- slope[[1L]] * signals$value + intercept[[1L]]

  gamma_error <- gamma_dose *
    sqrt((slope[[2L]] * sigma / slope[[1L]])^2 +
           (signals$error / signals$value)^2 +
           epsilon^2)

  results <- data.frame(
    names = signals$names,
    dose = gamma_dose,
    error = gamma_error,
    stringsAsFactors = FALSE
  )
  return(results)
}
