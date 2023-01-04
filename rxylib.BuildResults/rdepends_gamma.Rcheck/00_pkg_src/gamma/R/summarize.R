# SUMMARISE
#' @include AllClasses.R AllGenerics.R
NULL

# GammaSpectrum ================================================================
#' @rdname summarise
#' @aliases summarise,GammaSpectrum-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectrum",
  definition = function(object) {
    energy <- round(range_energy(object), 2)
    cbind.data.frame(
      name = object@name,
      date = as.character(object@date),
      live_time = object@live_time,
      real_time = object@real_time,
      channels = length(object),
      energy_min = energy[[1L]],
      energy_max = energy[[2L]],
      stringsAsFactors = FALSE
    )
  }
)

# GammaSpectra =================================================================
#' @rdname summarise
#' @aliases summarise,GammaSpectra-method
#' @export
setMethod(
  f = "summarise",
  signature = "GammaSpectra",
  definition = function(object) {
    sum_up <- lapply(X = object, FUN = summarise)
    do.call(rbind, sum_up)
  }
)

# DoseRateModel ================================================================
#' @rdname summarise
#' @aliases summarise,DoseRateModel-method
#' @export
setMethod(
  f = "summarise",
  signature = "DoseRateModel",
  definition = function(object) {
    list(
      residuals = get_residuals(object)$residuals,
      coefficients = matrix(data = c(object[["intercept"]], object[["slope"]]),
                            nrow = 2L, ncol = 2L, byrow = TRUE,
                            dimnames = list(c("Intercept", "Slope"),
                                            c("Estimate", "Std. Error"))),
      MSWD = object[["MSWD"]],
      df = object[["df"]],
      p_value = object[["p_value"]]
    )
  }
)

# CalibrationCurve =============================================================
#' @rdname summarise
#' @aliases summarise,CalibrationCurve-method
#' @export
setMethod(
  f = "summarise",
  signature = "CalibrationCurve",
  definition = function(object) {
    list(
      Ni = summarise(object[["Ni"]]),
      NiEi = summarise(object[["NiEi"]])
    )
  }
)
