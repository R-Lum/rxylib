# GETTERS AND SETTERS
#' @include AllClasses.R AllGenerics.R
NULL

# Getters ======================================================================
## GammaSpectrum ---------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases length,GammaSpectrum-method
setMethod(
  f = "length",
  signature = "GammaSpectrum",
  definition = function(x) length(x@channel)
)

#' @export
#' @rdname mutator
#' @aliases get_hash,GammaSpectrum-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectrum",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_names,GammaSpectrum-method
setMethod(
  f = "get_names",
  signature = "GammaSpectrum",
  definition = function(x) x@name
)

#' @export
#' @rdname mutator
#' @aliases get_livetime,GammaSpectrum-method
setMethod(
  f = "get_livetime",
  signature = "GammaSpectrum",
  definition = function(x) x@live_time
)

#' @export
#' @rdname mutator
#' @aliases get_realtime,GammaSpectrum-method
setMethod(
  f = "get_realtime",
  signature = "GammaSpectrum",
  definition = function(x) x@real_time
)

#' @export
#' @rdname mutator
#' @aliases get_channels,GammaSpectrum-method
setMethod(
  f = "get_channels",
  signature = "GammaSpectrum",
  definition = function(x) x@channel
)

#' @export
#' @rdname mutator
#' @aliases get_counts,GammaSpectrum-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectrum",
  definition = function(x) x@count
)

#' @export
#' @rdname mutator
#' @aliases get_rates,GammaSpectrum-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectrum",
  definition = function(x) x@rate
)

#' @export
#' @rdname mutator
#' @aliases get_energy,GammaSpectrum-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectrum",
  definition = function(x) x@energy
)

#' @export
#' @rdname mutator
#' @aliases range_energy,GammaSpectrum-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectrum",
  definition = function(x, na.rm = FALSE) {
    if (length(x@energy) == 0) return(c(NA, NA))
    range(x@energy, na.rm = na.rm)
  }
)

#' @export
#' @rdname mutator
#' @aliases range_channels,GammaSpectrum-method
setMethod(
  f = "range_channels",
  signature = "GammaSpectrum",
  definition = function(x, na.rm = FALSE) {
    if (length(x@channel) == 0) return(c(NA, NA))
    range(x@channel, na.rm = na.rm)
  }
)

## Baseline --------------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases set_method,Baseline-method
setMethod(
  f = "get_method",
  signature = "Baseline",
  definition = function(x) x@method
)

## GammaSpectra ----------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases get_hash,GammaSpectra-method
setMethod(
  f = "get_hash",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_hash, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_names,GammaSpectra-method
setMethod(
  f = "get_names",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_names, FUN.VALUE = character(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_livetime,GammaSpectra-method
setMethod(
  f = "get_livetime",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_livetime, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_realtime,GammaSpectra-method
setMethod(
  f = "get_realtime",
  signature = "GammaSpectra",
  definition = function(x) {
    vapply(x, FUN = get_realtime, FUN.VALUE = numeric(1))
  }
)

#' @export
#' @rdname mutator
#' @aliases get_channels,GammaSpectra-method
setMethod(
  f = "get_channels",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_channels)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_counts,GammaSpectra-method
setMethod(
  f = "get_counts",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_counts)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_rates,GammaSpectra-method
setMethod(
  f = "get_rates",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_rates)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_energy,GammaSpectra-method
setMethod(
  f = "get_energy",
  signature = "GammaSpectra",
  definition = function(x) {
    lapply(x, FUN = get_energy)
  }
)

#' @export
#' @rdname mutator
#' @aliases range_energy,GammaSpectra-method
setMethod(
  f = "range_energy",
  signature = "GammaSpectra",
  definition = function(x, na.rm = FALSE) {
    energy <- vapply(x, FUN = range_energy, FUN.VALUE = numeric(2),
                     na.rm = na.rm)
    energy <- t(energy)
    colnames(energy) <- c("min", "max")
    energy
  }
)

#' @export
#' @rdname mutator
#' @aliases range_channels,GammaSpectra-method
setMethod(
  f = "range_channels",
  signature = "GammaSpectra",
  definition = function(x, na.rm = FALSE) {
    channels <- vapply(x, FUN = range_channels, FUN.VALUE = numeric(2),
                      na.rm = na.rm)
    channels <- t(channels)
    colnames(channels) <- c("min", "max")
    channels
  }
)

## DoseRateModel ---------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases get_residuals,DoseRateModel-method
setMethod(
  f = "get_residuals",
  signature = "DoseRateModel",
  definition = function(x) {
    data <- x[["data"]]
    f <- gamma_dose ~ signal_value
    model <- stats::lm(f, data = data)

    n <- nrow(data)
    slope <- x[["slope"]][[1L]]
    intercept <- x[["intercept"]][[1L]]

    new_coef <- set_coefficients(f, c(intercept, slope), n)
    new_model <- stats::update(model, new_coef)

    data.frame(
      names = data$names,
      fitted = new_model$fitted.values,
      residuals = new_model$residuals,
      standardized = stats::rstandard(new_model),
      # cook = stats::cooks.distance(new_model),
      stringsAsFactors = FALSE
    )
  }
)

# https://stackoverflow.com/questions/12323859/how-to-manually-set-coefficients-for-variables-in-linear-model
set_coefficients <- function(f, coef, n){
  e1 <- paste0("offset(", coef[2], "*", as.character(f)[3], ")")
  e2 <- paste0("offset(rep(", coef[1], ",", n, "))")
  e <- paste(e1, e2, sep = " + ")
  stats::as.formula(paste(as.character(f)[2], "~", e, " -1"))
}

## PeakPosition ----------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases get_hash,PeakPosition-method
setMethod(
  f = "get_hash",
  signature = "PeakPosition",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_channels,PeakPosition-method
setMethod(
  f = "get_channels",
  signature = "PeakPosition",
  definition = function(x) x@channel
)

#' @export
#' @rdname mutator
#' @aliases get_energy,PeakPosition-method
setMethod(
  f = "get_energy",
  signature = "PeakPosition",
  definition = function(x, expected = FALSE) {
    if (expected) x@energy_expected else x@energy_observed
  }
)

# Setters ======================================================================
## GammaSpectrum ---------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases set_names,GammaSpectrum-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectrum",
  definition = function(x, value) {
    x@name <- as.character(value)
    methods::validObject(x)
    x
  }
)

## Baseline --------------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases set_method,Baseline-method
setMethod(
  f = "set_method<-",
  signature = "Baseline",
  definition = function(x, value) {
    x@method <- as.character(value)
    methods::validObject(x)
    x
  }
)

## GammaSpectra ----------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases set_name,GammaSpectra-method
setMethod(
  f = "set_names<-",
  signature = "GammaSpectra",
  definition = function(x, value) {
    names(x) <- value
    mapply(FUN = methods::`slot<-`, object = x, value = value,
           MoreArgs = list(name = "name"), SIMPLIFY = FALSE)
    methods::validObject(x)
    x
  }
)

## PeakPosition ----------------------------------------------------------------
#' @export
#' @rdname mutator
#' @aliases set_energy,PeakPosition,numeric-method
setMethod(
  f = "set_energy<-",
  signature = c(x = "PeakPosition", value = "numeric"),
  definition = function(x, value, expected = TRUE) {
    # Keep only complete cases
    k <- if (anyNA(value)) which(!is.na(value)) else seq_along(value)

    if (expected) {
      x@energy_expected <- value[k]
      x@energy_observed <- x@energy_observed[k]
    } else {
      x@energy_observed <- value[k]
      x@energy_expected <- x@energy_expected[k]
    }
    x@channel <- x@channel[k]
    methods::validObject(x)
    x
  }
)

