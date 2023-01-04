# INTEGRATE SIGNAL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectrum,missing-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectrum", background = "missing"),
  definition = function(object, range = NULL, energy = FALSE) {
    # Get data
    spc_data <- as.data.frame(object)

    # Validation
    if (!has_energy(object)) {
      stop("You must calibrate the energy scale of your spectrum first.",
           call. = FALSE)
    }
    if (is.null(range)) {
      range <- range(spc_data$energy)
    } else if (!is.numeric(range)) {
      stop(sprintf("%s must be a numeric vector, not %d.",
                   sQuote("range"), typeof(range)), call. = FALSE)
    } else if (length(range) != 2) {
      stop(sprintf("%s must be a numeric vector of length 2, not %d.",
                   sQuote("range"), length(range)), call. = FALSE)
    }

    # Integrate signal between boundaries
    spc_index <- which(spc_data$energy >= range[[1L]] &
                         spc_data$energy <= range[[2L]])
    if (energy) {
      int_spc <- integrate(spc_data$count, spc_data$energy, spc_index)
    } else {
      int_spc <- integrate(spc_data$count, index = spc_index)
    }

    # Normalize integrated signal to time
    live_time <- get_livetime(object)
    norm_signal <- int_spc / live_time
    norm_error <- sqrt(2 * int_spc) / live_time

    c(value = norm_signal, error = norm_error)
  }
)
#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectrum", background = "GammaSpectrum"),
  definition = function(object, background, range = NULL, energy = FALSE) {
    # Normalized signal
    int_bkg <- signal_integrate(background, range = range, energy = energy)

    signal_integrate(object, background = int_bkg, range = range,
                     energy = energy)
  }
)
#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectrum,numeric-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectrum", background = "numeric"),
  definition = function(object, background, range = NULL, energy = FALSE) {
    # Normalized signal
    int_spc <- signal_integrate(object, range = range, energy = energy)

    # Compute net signal (substracted background background)
    net_signal <- int_spc[[1L]] - background[[1L]]
    net_error <- sqrt(int_spc[[2L]]^2 + background[[2L]]^2)

    c(value = net_signal, error = net_error)
  }
)

#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectra,missing-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectra", background = "missing"),
  definition = function(object, range = NULL, energy = FALSE, simplify = TRUE) {

    signals <- lapply(X = object, FUN = signal_integrate,
                      range = range, energy = energy)
    if (simplify) do.call(rbind, signals) else signals
  }
)

#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectra,GammaSpectrum-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectra", background = "GammaSpectrum"),
  definition = function(object, background, range = NULL, energy = FALSE,
                        simplify = TRUE) {

    signals <- lapply(X = object, FUN = signal_integrate,
                      background = background, range = range, energy = energy)
    if (!simplify) return(signals)
    do.call(rbind, signals)
  }
)

#' @export
#' @rdname integrate
#' @aliases signal_integrate,GammaSpectra,numeric-method
setMethod(
  f = "signal_integrate",
  signature = signature(object = "GammaSpectra", background = "numeric"),
  definition = function(object, background, range = NULL, energy = FALSE,
                        simplify = TRUE) {

    signals <- lapply(X = object, FUN = signal_integrate,
                      background = background, range = range, energy = energy)
    if (!simplify) return(signals)
    do.call(rbind, signals)
  }
)

integrate <- function(count, energy, index) {
  if (missing(energy)) sum(count[index])
  else crossprod(energy[index], count[index])
}
