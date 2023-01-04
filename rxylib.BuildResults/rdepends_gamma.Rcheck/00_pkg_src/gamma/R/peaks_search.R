# SEARCH PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname peaks_search
#' @aliases peaks_search,GammaSpectrum,integer-method
setMethod(
  f = "peaks_search",
  signature = signature(object = "GammaSpectrum", index = "integer"),
  definition = function(object, index, span = 10, tolerance = 0.025) {
    ## Get data
    spc_channel <- get_channels(object)
    spc_counts <- get_counts(object)

    ## Search max value in energy +/- span range
    i <- search_peaks(index, x = spc_channel, y = spc_counts, span = span)

    ## Check diff between expected and observed peaks energy
    check_peaks(observed = spc_channel[i], expected = index,
                tolerance = tolerance)

    .PeakPosition(
      hash = object@hash,
      noise_method = "none",
      noise_threshold = numeric(0),
      window = span,
      channel = index
    )
  }
)

#' @export
#' @rdname peaks_search
#' @aliases peaks_search,GammaSpectrum,numeric-method
setMethod(
  f = "peaks_search",
  signature = signature(object = "GammaSpectrum", index = "numeric"),
  definition = function(object, index, span = 10, tolerance = 0.025) {
    ## Get data
    spc_channel <- get_channels(object)
    spc_counts <- get_counts(object)
    spc_energy <- get_energy(object)

    ## Search max value in energy +/- span range
    i <- search_peaks(index, x = spc_energy, y = spc_counts, span = span)

    ## Check diff between expected and observed peaks energy
    check_peaks(observed = spc_energy[i], expected = index,
                tolerance = tolerance)

    .PeakPosition(
      hash = object@hash,
      noise_method = "none",
      noise_threshold = numeric(0),
      window = span,
      channel = spc_channel[i],
      energy_observed = spc_energy[i],
      energy_expected = index
    )
  }
)

search_peaks <- function(index, x, y, span = 10) {
  ## Search max value in energy +/- span range
  # index <- index[order(index)] # Sort index values
  peaks <- numeric(length(index))

  for (i in seq_along(index)) {
    value <- index[[i]]
    found <- which(x >= value - span & x < value + span)
    k <- which.max(y[found])

    peaks[[i]] <- found[[k]]
  }

  peaks
}

check_peaks <- function(observed, expected, tolerance = 0.025) {
  ## Check diff between expected and observed peaks energy
  delta <- (observed - expected) / abs(expected)
  if (any(abs(delta) > tolerance)) {
    msg <- "The relative error on the peak positions is greater than %d%%.\n"
    warning(sprintf(msg, round(tolerance * 100)), call. = FALSE)
  }
  if (any(abs(delta) > 2 * tolerance)) {
    msg <- "The relative error on the peak positions is greater than %d%%.\n"
    stop(sprintf(msg, round(2 * tolerance * 100)), call. = FALSE)
  }
  invisible(TRUE)
}
