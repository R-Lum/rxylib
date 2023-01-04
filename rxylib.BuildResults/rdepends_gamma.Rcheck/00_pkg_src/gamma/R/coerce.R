# COERCION
#' @include AllClasses.R AllGenerics.R
NULL

# GammaSpectrum ================================================================
#' @method as.matrix GammaSpectrum
#' @rdname coerce
#' @export
as.matrix.GammaSpectrum <- function(x, ...) methods::as(x, "matrix")

#' @method as.data.frame GammaSpectrum
#' @export
as.data.frame.GammaSpectrum <- function(x, row.names = NULL, optional = FALSE,
                                        make.names = TRUE, ...,
                                        stringsAsFactors = FALSE) {
  x <- as.data.frame(x = as.matrix(x), row.names = row.names,
                     optional = optional, make.names = make.names, ...,
                     stringsAsFactors = stringsAsFactors)
  x
}

setAs(
  from = "GammaSpectrum",
  to = "matrix",
  def = function(from) {
    cbind(
      channel = if (length(from@channel) != 0) from@channel else NA_real_,
      energy = if (length(from@energy) != 0) from@energy else NA_real_,
      count = if (length(from@count) != 0) from@count else NA_real_,
      rate = if (length(from@rate) != 0) from@rate else NA_real_
    )
  }
)
setAs(
  from = "GammaSpectrum",
  to = "data.frame",
  def = function(from) {
    m <- as.matrix(from)
    m <- as.data.frame(m, stringsAsFactors = FALSE)
    m
  }
)

# GammaSpectra =================================================================
as_long <- function(from) {
  df_list <- lapply(X = from, FUN = "as", Class = "data.frame")
  df_nrow <- vapply(X = df_list, FUN = nrow, FUN.VALUE = integer(1))
  df_long <- do.call(rbind, df_list)

  ## Keep original ordering
  name <- rep(names(df_list), times = df_nrow)
  df_long$name <- factor(name, levels = unique(name))
  rownames(df_long) <- NULL

  return(df_long)
}

setAs(
  from = "GammaSpectrum",
  to = "GammaSpectra",
  def = function(from) .GammaSpectra(list(from))
)
setAs(
  from = "list",
  to = "GammaSpectra",
  def = function(from) {
    k <- lapply(X = from, FUN = get_names)
    names(from) <- unlist(k)
    .GammaSpectra(from)
  }
)

# PeakPosition =================================================================
#' @method as.matrix PeakPosition
#' @export
as.matrix.PeakPosition <- function(x, ...) methods::as(x, "matrix")

#' @method as.data.frame PeakPosition
#' @export
as.data.frame.PeakPosition <- function(x, row.names = NULL, optional = FALSE,
                                        make.names = TRUE, ...,
                                        stringsAsFactors = FALSE) {
  x <- as.data.frame(x = as.matrix(x), row.names = row.names,
                     optional = optional, make.names = make.names, ...,
                     stringsAsFactors = stringsAsFactors)
  x
}

setAs(
  from = "PeakPosition",
  to = "matrix",
  def = function(from) {
    cbind(
      channel = if (length(from@channel) != 0) from@channel else NA_real_,
      energy_observed = if (length(from@energy_observed) != 0) from@energy_observed else NA_real_,
      energy_expected = if (length(from@energy_expected) != 0) from@energy_expected else NA_real_
    )
  }
)
setAs(
  from = "PeakPosition",
  to = "data.frame",
  def = function(from) {
    m <- as.matrix(from)
    m <- as.data.frame(m, stringsAsFactors = FALSE)
    m
  }
)
