# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname peaks_find
#' @aliases peaks_find,GammaSpectrum-method
setMethod(
  f = "peaks_find",
  signature = signature(object = "GammaSpectrum"),
  definition = function(object, method = c("MAD"), SNR = 2, span = NULL, ...) {
    # Get count data
    spc <- as.data.frame(object)
    count <- spc$count

    # Validation
    method <- match.arg(method, several.ok = FALSE)
    SNR <- as.integer(SNR)[[1L]]
    if (is.null(span)) span <- round(length(count) * 0.05)
    span <- as.integer(span)[[1L]]

    if (SNR != 0) {
      noise <- switch (
        method,
        MAD = MAD
      )
      threshold <- noise(count, ...) * SNR
      index_noise <- count < threshold
      count[index_noise] <- 0
    }

    shape <- diff(sign(diff(count, na.pad = FALSE)))
    index_shape <- lapply(
      X = which(shape < 0),
      FUN = function(i, data, span) {
        n <- length(data)
        z <- i - span + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + span + 1
        w <- ifelse(w < n, w, n)
        if (all(data[c(z:i, (i + 2):w)] <= data[i + 1])) {
          return(i + 1)
        } else {
          return(numeric(0))
        }
      },
      data = count,
      span = span
    )
    index_noise <- unlist(index_shape)

    pks <- spc[index_noise, ]
    rownames(pks) <- paste0("peak #", seq_len(nrow(pks)))

    .PeakPosition(
      hash = object@hash,
      noise_method = method,
      noise_threshold = threshold,
      window = span,
      channel = pks$channel,
      energy_observed = pks$energy
    )
  }
)

#' MAD
#'
#' Calculates the Median Absolute Deviation (MAD).
#' @param x A [`numeric`] vector.
#' @param k A [`numeric`] value.
#' @param na.rm A [`logical`] scalar.
#' @return A `numeric` value.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
MAD <- function(x, k = 1.4826, na.rm = FALSE) {
  k * stats::median(abs(x - stats::median(x, na.rm = na.rm)), na.rm = na.rm)
}

#' FWHM
#'
#' Estimates the Half-Width at Half-Maximum (FWHM) for a given peak.
#' @param x,y A [`numeric`] vector giving the `x` and `y` coordinates of a set
#'  of points. Alternatively, a single argument `x` can be provided.
#' @param center A [`numeric`] value giving the peak position in `x` units.
#' @return A [`numeric`] value.
#' @details
#'  It tries to get the smallest possible estimate.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
FWHM <- function(x, y, center) {
  if (missing(y)) {
    z <- x
    if (is.list(z)) {
      x <- z[[1]]
      y <- z[[2]]
    }
    if (is.matrix(z) | is.data.frame(z)) {
      x <- z[, 1]
      y <- z[, 2]
    }
  } else {
    if (length(x) != length(y))
      stop("`x` and `y` lengths differ.", call. = FALSE)
  }

  i <- which(x == center)
  peak_height <- y[i]
  scale_for_roots <- y - peak_height / 2
  root_indices <- which(diff(sign(scale_for_roots)) != 0)

  tmp <- sort(c(root_indices, i))
  k <- which(tmp == i)

  root_left <- root_indices[k - 1]
  root_right <- root_indices[k]

  HWHM_left <- x[i] - x[root_left]
  HWHM_right <- x[root_right] - x[i]

  FWHM <- 2 * min(c(HWHM_left, HWHM_right))
  return(FWHM)
}
