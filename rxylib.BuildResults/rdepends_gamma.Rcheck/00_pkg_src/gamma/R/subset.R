# SUBSET
#' @include AllClasses.R AllGenerics.R
NULL

extract_slot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  data
}

# GammaSpectrum ================================================================
#' @export
#' @rdname subset
#' @aliases [[,GammaSpectrum-method
setMethod(
  f = "[[",
  signature = "GammaSpectrum",
  definition = extract_slot
)

# GammaSpectra =================================================================
#' @export
#' @rdname subset
#' @aliases [,GammaSpectra-method
setMethod(
  f = "[",
  signature = "GammaSpectra",
  definition = function(x, i, j) {
    spc_list <- x@.Data
    names(spc_list) <- names(x) # Fix names

    if (missing(i)) {
      i <- seq_along(spc_list)
    } else {
      if (is.null(i)) i <- seq_along(spc_list)
      if (is.character(i) | is.factor(i)) i <- which(names(spc_list) %in% i)
      if (is.numeric(i)) i <- as.integer(i)
    }

    # Select spectra
    items <- spc_list[i]

    if (!missing(j)) {
      # Select slot
      slot <- lapply(X = items, FUN = "[[", j)
      # if (!drop)
      #   slot <- as.data.frame(slot, fix.empty.names = TRUE)
      return(slot)
    } else {
      methods::new("GammaSpectra", items)
    }
  }
)

# DoseRateModel ================================================================
#' @export
#' @rdname subset
#' @aliases [[,DoseRateModel-method
setMethod(
  f = "[[",
  signature = "DoseRateModel",
  definition = extract_slot
)

# CalibrationCurve =============================================================
#' @export
#' @rdname subset
#' @aliases [[,CalibrationCurve-method
setMethod(
  f = "[[",
  signature = "CalibrationCurve",
  definition = extract_slot
)

# PeakPosition =================================================================
#' @export
#' @rdname subset
#' @aliases [[,PeakPosition-method
setMethod(
  f = "[[",
  signature = "PeakPosition",
  definition = extract_slot
)
