# FIT DOSE RATE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname doserate
#' @aliases dose_fit,GammaSpectra,GammaSpectrum,matrix-method
setMethod(
  f = "dose_fit",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum",
                        doses = "matrix"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        details = list(authors = "", date = Sys.time())) {
    doses <- as.data.frame(doses)
    methods::callGeneric(object, background, doses, range_Ni = range_Ni,
                         range_NiEi = range_NiEi, details = details)
  }
)

#' @export
#' @rdname doserate
#' @aliases dose_fit,GammaSpectra,data.frame-method
setMethod(
  f = "dose_fit",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum",
                        doses = "data.frame"),
  definition = function(object, background, doses, range_Ni, range_NiEi,
                        details = list(authors = "", date = Sys.time())) {
    # Validation
    if (length(range_Ni) != 2 | length(range_NiEi) != 2)
      stop(sprintf("%s must be of length 2.", sQuote("range_*")), call. = FALSE)
    if (is.null(rownames(doses)))
      stop(sprintf("%s is missing row names.", sQuote("doses")), call. = FALSE)
    doses <- doses[, c(1, 2)]

    # Metadata
    info <- if (is.list(details)) details else list()
    if (is.null(info$date))
      info$date <- Sys.time()

    # Fit linear regression (York)
    Ni <- fit_york(object, background, doses, range = range_Ni, energy = FALSE)
    NiEi <- fit_york(object, background, doses, range = range_NiEi, energy = TRUE)

    .CalibrationCurve(
      Ni = Ni,
      NiEi = NiEi,
      details = info
    )
  }
)

fit_york <- function(object, background, doses, range, energy = FALSE) {
  # Signal integration
  bkg <- signal_integrate(background, range = range, energy = energy)
  signals <- signal_integrate(object, background = bkg, range = range,
                              energy = energy, simplify = TRUE)

  # Prepare data
  data <- merge(signals, doses, by = 0, all = FALSE, sort = FALSE)
  colnames(data) <- c("names", "signal_value", "signal_error",
                      "gamma_dose", "gamma_error")

  # Fit model
  model <- IsoplotR::york(data[, -1])
  # fitted <- model$a[[1L]] + data$signal_value * model$b[[1L]]
  # residuals <- data$gamma_dose - fitted
  # names(residuals) <- seq_along(residuals)

  .DoseRateModel(
    slope = as.numeric(model$b),
    intercept = as.numeric(model$a),
    covariance = model$cov.ab,
    MSWD = model$mswd,
    df = model$df,
    p_value = model$p.value,
    data = data,
    range = range,
    background = bkg
  )
}
