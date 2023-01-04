# CLASSES DEFINITION AND INITIALIZATION

# Class Union ==================================================================
setClassUnion("LmOrNull", c("lm", "NULL"))

# GammaSpectrum ================================================================
#' An S4 Class to Represent a Gamma Sectrum
#'
#' Represents a single spectrum of a gamma ray spectrometry measurement.
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of
#'  the imported file.
#' @slot name A [`character`] string the measurement reference.
#' @slot date A [`POSIXct`] element giving the measurement date and time.
#' @slot instrument A [`character`] string giving the instrument name.
#' @slot file_format A [`character`] string giving the format of the
#'  imported file.
#' @slot live_time A [`numeric`] value.
#' @slot real_time A [`numeric`] value.
#' @slot channel A [`integer`] vector giving the channel number.
#'  Numeric values are coerced to integer as by [as.integer()]
#'  (and hence truncated towards zero).
#' @slot energy A [`numeric`] vector giving the gamma ray's energy (in keV).
#' @slot count A [`numeric`] vector giving the counts number for each channel.
#'  Numeric values are coerced to integer as by [as.integer()] (and hence
#'  truncated towards zero).
#' @slot rate A [`numeric`] vector the count rate (\eqn{1/s}) for each channel.
#' @slot calibration A [linear model][stats::lm()] used for energy scale
#'  calibration (see [energy_calibrate()]).
#' @section Access:
#' In the code snippets below, `x` is a `GammaSpectrum` object.
#' \describe{
#'  \item{`length(x)`}{Get number of channel in `x`.}
#'  \item{`get_hash(x)`}{Get the MD5 hash of the raw data file.}
#'  \item{`get_names(x)`, `set_names(x) <- value`}{Retrieves or sets
#'   the name of `x` according to `value`.}
#'  \item{`get_channels(x)`}{Get the number of channels in `x`.}
#'  \item{`get_counts(x)`}{Get the counts of `x`.}
#'  \item{`get_energy(x)`}{Get the energy range of `x`.}
#'  \item{`get_rates(x)`}{Get the count rates of `x`.}
#' }
#' @section Coerce:
#' In the code snippets below, `x` is a `GammaSpectrum` object.
#' \describe{
#'  \item{`as.matrix(x)`}{Coerces `x` to a [`matrix`].}
#'  \item{`as.data.frame(x)`}{Coerces `x` to a [`data.frame`].}
#' }
#' @section Subset:
#' In the code snippets below, `x` is a `GammaSpectrum` object.
#' \describe{
#'  \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'  `i` is a `character` vector of length one and will be matched to the name of
#'  the slots.}
#' }
#' @note This class retains copy construction.
#' @example inst/examples/ex-GammaSpectrum.R
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases GammaSpectrum-class
.GammaSpectrum <- setClass(
  Class = "GammaSpectrum",
  slots = c(
    hash = "character",
    name = "character",
    date = "POSIXct",
    instrument = "character",
    file_format = "character",
    channel = "integer",
    energy = "numeric",
    count = "numeric",
    rate = "numeric",
    live_time = "numeric",
    real_time = "numeric",
    calibration = "LmOrNull"
  ),
  prototype = list(
    hash = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    name = character(0),
    date = Sys.time(),
    instrument = character(0),
    file_format = character(0),
    channel = integer(0),
    energy = numeric(0),
    count = numeric(0),
    rate = numeric(0),
    live_time = numeric(0),
    real_time = numeric(0),
    calibration = NULL
  )
)

# GammaSpectra =================================================================
#' An S4 Class to Represent a Collection of Gamma Sectra
#'
#' Represents a collection of spectra of gamma ray spectrometry measurements.
#' @details
#'  This class extends the base [`list`] and can only contains
#'  [GammaSpectrum-class] objects.
#' @section Access:
#' In the code snippets below, `x` is a `GammaSpectra` object.
#' \describe{
#'  \item{`length(x)`}{Get the number of elements in `x`.}
#'  \item{`lengths(x)`}{Get the number of channels in each element of `x`.}
#'  \item{`get_names(x)`, `set_names(x) <- value`}{Retrieves or sets
#'   the names of `x` according to `value`.}
#'  \item{`get_hash(x)`}{Get the MD5 hash of the raw data files.}
#'  \item{`get_channels(x)`}{Get the number of channels in each element of `x`.}
#'  \item{`get_counts(x)`}{Get the counts of each element of `x`.}
#'  \item{`get_energy(x)`}{Get the energy range of each element of `x`.}
#'  \item{`get_rates(x)`}{Get the count rates of each element of `x`.}
#' }
#' @section Subset:
#' In the code snippets below, `x` is a `GammaSpectra` object.
#' \describe{
#'  \item{`x[i]`}{Extracts the elements selected by subscript `i`.
#'   `i` can be `missing` or `NULL`, `numeric` or `character` vector or a
#'   `factor`. Returns a new `GammaSpectra` object.}
#'  \item{`x[i, j]`}{Like the above but allows to select a slot thru `j` (see
#'   examples). `j` is a `character` vector of length one. Returns a `list`.}
#'  \item{`x[[i]]`}{Extracts the elements selected by subscript `i`.
#'   `i` can be a `numeric` or `character` vector of length one. Returns the
#'   corresponding [GammaSpectrum-class] object.}
#' }
#' @example inst/examples/ex-GammaSpectra.R
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases GammaSpectra-class
.GammaSpectra <- setClass(
  Class = "GammaSpectra",
  contains = "list"
)

# Baseline =====================================================================
#' An S4 Class to Represent a Spectrum Baseline
#'
#' @note This class extends the [GammaSpectrum-class] class.
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases BaseLine-class
.Baseline <- setClass(
  Class = "Baseline",
  slots = c(
    method = "character"
  ),
  contains = "GammaSpectrum",
  prototype = list(
    method = "unknown"
  )
)

# CalibrationCurve =============================================================
#' An S4 class to Represent a Dose Rate Calibration Curve
#'
#' @slot Ni A [DoseRateModel-class] object.
#' @slot NiEi A [DoseRateModel-class] object.
#' @slot details A [`list`] of length-one vector giving the curve metadata.
#' @slot slope A [`numeric`] vector.
#' @slot intercept A [`numeric`] vector.
#' @slot covariance A [`numeric`] vector.
#' @slot MSWD A [`numeric`] vector.
#' @slot df A [`numeric`] vector.
#' @slot p_value A [`numeric`] vector.
#' @slot data A [`data.frame`].
#' @slot range A [`numeric`] vector.
#' @slot background A [`numeric`] vector.
#' @section Subset:
#' In the code snippets below, `x` is a `CalibrationCurve` object.
#' \describe{
#'  \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'  `i` is a `character` vector of length one.}
#' }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name CalibrationCurve-class
#' @rdname CalibrationCurve-class
NULL

#' @aliases DoseRateModel-class
#' @rdname CalibrationCurve-class
.DoseRateModel <- setClass(
  Class = "DoseRateModel",
  slots = c(
    slope = "numeric",
    intercept = "numeric",
    covariance = "numeric",
    MSWD = "numeric",
    df = "numeric",
    p_value = "numeric",
    data = "data.frame",
    range = "numeric",
    background = "numeric"
  )
)

#' @aliases DoseRateModel-class
#' @rdname CalibrationCurve-class
.CalibrationCurve <- setClass(
  Class = "CalibrationCurve",
  slots = c(
    Ni = "DoseRateModel",
    NiEi = "DoseRateModel",
    details = "list"
  )
)

# PeakPosition =================================================================
#' An S4 Class to Represent a Set of Peaks
#'
#' @slot hash A [`character`] string giving the 32-byte MD5 hash of the imported
#'  spectrum file.
#' @slot noise_method A [`character`] string specifying the method used for peak
#'  detection.
#' @slot noise_threshold A length one [`numeric`] vector giving the noise
#'  threshold.
#' @slot window A length one [`numeric`] vector giving the half-window size.
#' @slot channel A [`integer`] vector giving the channel number. Numeric values
#'  are coerced to integer as by [as.integer()] (and hence truncated towards
#'  zero).
#' @slot energy_observed A [`numeric`] vector giving the observed gamma ray
#'  energy (in keV).
#' @slot energy_expected A [`numeric`] vector giving the expected gamma ray
#'  energy (in keV).
#' @section Access:
#' In the code snippets below, `x` is a `PeakPosition` object.
#' \describe{
#'  \item{`get_hash(x)`}{Get the MD5 hash of the raw data file.}
#'  \item{`get_channels(x)`}{Get the channels of `x`.}
#'  \item{`get_energy(x)`, `set_energy(x) <- value`}{Retrieves or sets
#'   the energy scale of `x` according to `value`.}
#' }
#' @section Coerce:
#' In the code snippets below, `x` is a `PeakPosition` object.
#' \describe{
#'  \item{`as.matrix(x)`}{Coerces `x` to a [`matrix`].}
#'  \item{`as.data.frame(x)`}{Coerces `x` to a [`data.frame`].}
#' }
#' @section Subset:
#' In the code snippets below, `x` is a `PeakPosition` object.
#' \describe{
#'  \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'  `i` is a `character` vector of length one and will be matched to the name of
#'  the slots.}
#' }
#' @note This class retains copy construction.
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @aliases PeakPosition-class
.PeakPosition <- setClass(
  Class = "PeakPosition",
  slots = list(
    hash = "character",
    noise_method = "character",
    noise_threshold = "numeric",
    window = "numeric",
    channel = "integer",
    energy_observed = "numeric",
    energy_expected = "numeric"
  ),
  prototype = list(
    hash = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    noise_method = character(0),
    noise_threshold = numeric(0),
    window = integer(0),
    channel = integer(0),
    energy_observed = numeric(0),
    energy_expected = numeric(0)
  )
)
