# HELPERS

#' Reverse Cumulative Sum
#'
#' @param x A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
reverse_cumsum <- function(x) {
  rev(cumsum(rev(x)))
}

#' Equality within a vector
#'
#' Checks for equality among all elements of a vector.
#' @param x A [`numeric`] vector to be checked.
#' @param tolerance A length-one \link{`numeric`} vector giving the
#'  tolerance to check within.
#' @param na.rm A [`logical`] scalar specifying if missing values
#'  (including `NaN`) should be omitted.
#' @return A [`logical`].
#' @keywords internal
#' @noRd
isEqual <- function(x, tolerance = .Machine$double.eps^0.5, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.", call. = FALSE)
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) <= tolerance
}

#' Positive numbers
#'
#' Checks if an object only contains positive values.
#' @param x A [`numeric`] object to be checked.
#' @param strict A [`logical`] scalar.
#' @param na.rm A [`logical`] scalar specifying if missing values
#'  (including `NaN`) should be omitted.
#' @return A [`logical`].
#' @keywords internal
#' @noRd
isPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  # Validation
  if (!is.numeric(x))
    stop("A numeric vector is expected.", call. = FALSE)

  if (strict) {
    !any(x <= 0, na.rm = na.rm)
  } else {
    !any(x < 0, na.rm = na.rm)
  }
}
