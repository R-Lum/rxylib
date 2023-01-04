# WEIGHTED SMOOTHING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname smooth
#' @aliases smooth_savitzky,GammaSpectrum-method
setMethod(
  f = "smooth_savitzky",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, m = 3, p = 2, ...) {
    # Get data
    x <- get_counts(object)
    z <- savitzky(x, m = m, p = p)

    methods::initialize(object, count = z)
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_savitzky,GammaSpectra-method
setMethod(
  f = "smooth_savitzky",
  signature = signature(object = "GammaSpectra"),
  definition = function(object, m = 3, p = 2, ...) {
    spc <- lapply(X = object, FUN = smooth_savitzky, m = m, p = p)
    .GammaSpectra(spc)
  }
)

#' Savitzky-Golay Filter
#'
#' \code{smooth_savitzky} smoothes the data using the Savitzky-Golay filter.
#'
#' \code{coef_savitzky} computes the Savitzky-Golay convolution coefficients.
#' @param x A [`numeric`] vector of observed values to be smoothed.
#' @param m An odd [`integer`] scalar giving the number of adjacent
#'  points to use.
#' @param p An [`integer`] scalar giving the polynomial degree.
#' @return A [`numeric`] vector.
#' @name smooth_savitzky
#' @rdname smooth_savitzky
#' @keywords internal
#' @noRd
savitzky <- function(x, m, p = 2) {
  # Validation
  m <- as.integer(m)[[1L]]
  if (m %% 2 == 0)
    stop(sQuote("m"), " must be an odd integer.", call. = FALSE)

  k <- (m - 1) / 2
  i <- seq(from = -k, to = k, by = 1)
  j <- utils::head(utils::tail(seq_along(x), n = -k), n = -k)
  conv <- coef_savitzky(m, p)

  smoothed <- vapply(
    X = j,
    FUN = function(j, i, conv, data) {
      sum(conv * data[j + i])
    },
    FUN.VALUE = double(1),
    i = i,
    conv = conv,
    data = x
  )
  x[j] <- smoothed
  x
}

coef_savitzky <- function(m, p = 2) {
  k <- (m - 1) / 2
  z <- seq(from = -k, to = k, by = 1)
  J <- vapply(X = c(0, p), FUN = function(p, z) z^p, z, FUN.VALUE = double(m))
  (solve(t(J) %*% J) %*% t(J))[1, , drop = TRUE]
}
