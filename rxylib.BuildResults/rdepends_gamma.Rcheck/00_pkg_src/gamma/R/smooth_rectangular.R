# UNWEIGHTED SMOOTHING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname smooth
#' @aliases smooth_rectangular,GammaSpectrum-method
setMethod(
  f = "smooth_rectangular",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, m = 3, ...) {
    # Get data
    x <- get_counts(object)
    z <- rectangular(x, m = m)

    methods::initialize(object, count = z)
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_rectangular,GammaSpectra-method
setMethod(
  f = "smooth_rectangular",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, m = 3, ...) {
    spc <- lapply(X = object, FUN = smooth_rectangular, m = m)
    .GammaSpectra(spc)
  }
)

#' Rectangular Smooth
#'
#' Unweighted sliding-average.
#' @param x A [`numeric`] vector of observed values to be smoothed.
#' @param m An odd [`integer`] scalar giving the number of adjacent
#'  points to use.
#' @return A [`numeric`] vector of the same length as `x`.
#' @keywords internal
#' @noRd
rectangular <- function(x, m) {
  # Validation
  m <- as.integer(m)[[1L]]
  if (m %% 2 == 0)
    stop(sQuote("m"), " must be an odd integer.", call. = FALSE)

  # Index
  k <- (m - 1) / 2
  index_k <- seq_len(k)
  index_x <- seq_along(x)
  index_m <- c(index_k, rep_len(k + 1, length(x) - 2 * k), rev(index_k)) - 1

  smoothed <- mapply(
    FUN = function(i, k, data) {
      index <- seq(from = i - k, to = i + k, by = 1)
      mean(data[index])
    },
    i = index_x,
    k = index_m,
    MoreArgs = list(data = x)
  )
  smoothed
}
