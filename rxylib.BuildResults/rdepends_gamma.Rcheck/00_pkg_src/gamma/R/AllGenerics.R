# GENERIC METHODS
#' @include AllClasses.R
NULL

# Add S4 dispatch to base S3 generic
setGeneric("length")

# Coerce =======================================================================
#' Coerce
#'
#' @param x An object to be coerced.
#' @param ... Currently not used.
#' @return A coerced object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family class
#' @name coerce
#' @rdname coerce
NULL

# Extract ======================================================================
# Getters ----------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of `x`.
#' @param expected TODO.
#' @param na.rm A [`logical`] scalar: should [`NA`] be omitted?
#' @param ... Currently not used.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_hash-method
setGeneric(
  name = "get_hash",
  def = function(x) standardGeneric("get_hash")
)

#' @rdname mutator
#' @aliases get_names-method
setGeneric(
  name = "get_names",
  def = function(x) standardGeneric("get_names")
)

#' @rdname mutator
#' @aliases set_names-method
setGeneric(
  name = "set_names<-",
  def = function(x, value) standardGeneric("set_names<-")
)

#' @rdname mutator
#' @aliases get_livetime-method
setGeneric(
  name = "get_livetime",
  def = function(x) standardGeneric("get_livetime")
)

#' @rdname mutator
#' @aliases get_realtime-method
setGeneric(
  name = "get_realtime",
  def = function(x) standardGeneric("get_realtime")
)

#' @rdname mutator
#' @aliases get_channels-method
setGeneric(
  name = "get_channels",
  def = function(x) standardGeneric("get_channels")
)

#' @rdname mutator
#' @aliases get_counts-method
setGeneric(
  name = "get_counts",
  def = function(x) standardGeneric("get_counts")
)

#' @rdname mutator
#' @aliases get_rates-method
setGeneric(
  name = "get_rates",
  def = function(x) standardGeneric("get_rates")
)

#' @rdname mutator
#' @aliases get_energy-method
setGeneric(
  name = "get_energy",
  def = function(x, ...) standardGeneric("get_energy")
)

#' @rdname mutator
#' @aliases set_energy-method
setGeneric(
  name = "set_energy<-",
  def = function(x, ..., value) standardGeneric("set_energy<-")
)

#' @rdname mutator
#' @aliases get_method-method
setGeneric(
  name = "get_method",
  def = function(x) standardGeneric("get_method")
)

#' @rdname mutator
#' @aliases set_method-method
setGeneric(
  name = "set_method<-",
  def = function(x, value) standardGeneric("set_method<-")
)

#' @rdname mutator
#' @aliases get_residuals-method
setGeneric(
  name = "get_residuals",
  def = function(x) standardGeneric("get_residuals")
)

#' @rdname mutator
#' @aliases range_channels-method
setGeneric(
  name = "range_channels",
  def = function(x, ...) standardGeneric("range_channels")
)

#' @rdname mutator
#' @aliases range_energy-method
setGeneric(
  name = "range_energy",
  def = function(x, ...) standardGeneric("range_energy")
)

# Subset -----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to
#'  replace element(s).
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  [`numeric`], [`integer`] or [`character`] vectors or empty (missing) or
#'  `NULL`. Numeric values are coerced to [`integer`] as by [as.integer()] (and
#'  hence truncated towards zero). Character vectors will be matched to the name
#'  of the elements. An empty index (a comma separated blank) indicates that all
#'  entries in that dimension are selected.
# @param drop A [`logical`] scalar: should the result be coerced to the lowest
#  possible dimension? This only works for extracting elements, not for the
#  replacement.
#' @return
#'  A subsetted object.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Operators ====================================================================
#' Common Operations on GammaSpectrum Objects
#'
#' Performs common operations on `GammaSpectrum` objects.
#' @param x,e1,e2 An object (typically a [GammaSpectrum-class] object).
#' @param digits A length-one [`numeric`] vector giving the number of digits to
#'  be used in [round()] or [signif()].
#' @param na.rm A [`logical`] scalar: should missing values (including `NaN`) be
#'  omitted from the calculations?
#' @param ... Further arguments passed to or from methods.
#' @section Group Generics:
#'  [GammaSpectrum-class] objects have support for S4 group generic
#'  functionality to operate within elements across objects:
#'  \describe{
#'   \item{`Arith`}{"`+`", "`-`", "`*`", "`^`", "`\%\%`", "`\%/\%`", "`/`"}
#'   \item{`Compare`}{"`==`", "`>`", "`<`", "`!=`", "`<=`", "`>=`"}
#'   \item{`Logic`}{"`&`", "`|`"}
#'   \item{`Math`}{"`abs`", "`sign`", "`sqrt`", "`ceiling`", "`floor`",
#'   "`trunc`", "`cummax`", "`cummin`", "`cumprod`", "`cumsum`", "`log`",
#'   "`log10`", "`log2`", "`log1p`", "`acos`", "`acosh`", "`asin`", "`asinh`",
#'   "`atan`", "`atanh`", "`exp`", "`expm1`", "`cos`", "`cosh`", "`cospi`",
#'   "`sin`", "`sinh`", "`sinpi`", "`tan`", "`tanh`", "`tanpi`", "`gamma`",
#'   "`lgamma`", "`digamma`", "`trigamma`"}
#'   \item{`Math2`}{"`round`", "`signif`"}
#'   \item{`Ops`}{"`Arith`", "`Compare`", "`Logic`"}
#'   \item{`Summary`}{"`min`", "`max`", "`range`", "`prod`", "`sum`", "`any`",
#'   "`all`"}
#'  }
#' @example inst/examples/ex-operators.R
#' @author N. Frerebeau
#' @docType methods
#' @family operator
#' @name operator
#' @rdname operator
NULL

# Energy scale =================================================================
#' Energy Scale Calibration
#'
#' Calibrates the energy scale of a gamma spectrum.
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class] object.
#' @param lines A [PeakPosition-class] object or a [`list`] of length two. If a
#'  `list` is provided, each element must be a named numeric vector giving the
#'  observed peak position ("`channel`") and the corresponding expected
#'  "`energy`" value (in keV).
#' @param ... Currently not used.
#' @details
#'  The energy calibration of a spectrum is the most tricky part. To do this,
#'  the user must specify the position of at least three observed peaks and the
#'  corresponding energy value (in keV). A second order polynomial model is
#'  fitted on these energy *vs* channel values, then used to predict the
#'  new energy scale of the spectrum.
#'
#'  The package allows to provide the channel-energy pairs to be use. However,
#'  the spectrum can be noisy so it is difficult to properly determine the peak
#'  channel. In this case, a better approach may be to pre-process the spectrum
#'  (variance-stabilization, smoothing and baseline correction) and perform a
#'  peak detection. Once the identified peaks are satisfactory, you can set the
#'  corresponding energy values (in keV) and use these lines to calibrate the
#'  energy scale of the spectrum.
#'
#'  Regardless of the approach you choose, it is strongly recommended to check
#'  the result before proceeding.
#' @return
#'  * `energy_calibrate()` returns a [GammaSpectrum-class] object.
#'  * `has_energy()` and `has_calibration()` return a [`logical`] vector.
#' @example inst/examples/ex-energy.R
#' @author N. Frerebeau
#' @docType methods
#' @family energy
#' @name energy
#' @rdname energy
NULL

#' @rdname energy
#' @aliases energy_calibrate-method
setGeneric(
  name = "energy_calibrate",
  def = function(object, lines, ...) standardGeneric("energy_calibrate")
)

#' @rdname energy
setGeneric(
  name = "has_energy",
  def = function(object) standardGeneric("has_energy")
)

#' @rdname energy
setGeneric(
  name = "has_calibration",
  def = function(object) standardGeneric("has_calibration")
)

# Baseline =====================================================================
#' Baseline Estimation and Removal
#'
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class]
#'  object.
#' @param method A [`character`] string specifying the method to be used for
#'  baseline estimation (see details). Any unambiguous substring can be given.
#' @param LLS A [`logical`] scalar: should the LLS operator be applied on `x`
#'  before employing SNIP algorithm? Only used if `method` is "`SNIP`".
#' @param decreasing A [`logical`] scalar: should a decreasing clipping window
#'  be used? Only used if `method` is "`SNIP`".
#' @param n An [`integer`] value giving the number of iterations.
#'  Only used if `method` is "`SNIP`".
#' @param noise A length-one [`numeric`] vector giving the noise level.
#'  Only used if `method` is "`rubberband`".
#' @param spline A [`logical`] scalar: should spline interpolation through the
#'  support points be used instead of linear interpolation?
#'  Only used if `method` is "`rubberband`".
#' @param from An [`integer`] giving the first channel to be used for linear
#'  interpolation. If `NULL` (the default), channel \eqn{1} is used.
#'  Only used if `method` is "`linear`".
#' @param to An [`integer`] giving the last channel to be used for linear
#'  interpolation. If `NULL` (the default), channel \eqn{max} is used.
#'  Only used if `method` is "`linear`".
#' @param ... Extra parameters to be passed to further methods.
#' @details
#'  The following methods are available for baseline estimation:
#'  \describe{
#'   \item{`SNIP`}{Sensitive Nonlinear Iterative Peak clipping algorithm.}
#'   \item{`rubberband`}{A convex envelope of the spectrum is determined and the
#'   baseline is estimated as the part of the convex envelope lying below the
#'   spectrum. Note that the rubber band does not enter the concave regions
#'   (if any) of the spectrum.}
#'   \item{`linear`}{Linear baseline estimation.}
#'  }
#' @note
#'  `baseline_rubberband()` is slightly modified from C. Beleites'
#'  [hyperSpec::spc.rubberband()].
#' @return
#'  * `baseline_*()` returns a [BaseLine-class] object.
#'  * `signal_correct()` returns a corrected [GammaSpectrum-class] or
#'    [GammaSpectra-class] object (same as `object`).
#' @references
#'  Liland, K. H. (2015). 4S Peak Filling - baseline estimation by iterative
#'  mean suppression. *MethodsX*, 2, 135-140. \doi{10.1016/j.mex.2015.02.009}.
#'
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. & Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  *Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment*, 401(1),
#'  p. 113-132. \doi{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. & Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. *Applied Spectroscopy*, 62(1), p. 91-106.
#'  \doi{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. & Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  *Nuclear Instruments and Methods in Physics Research Section B:
#'  Beam Interactions with Materials and Atoms*, 34(3), p. 396-402.
#'  \doi{10.1016/0168-583X(88)90063-8}
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @name baseline
#' @rdname baseline
NULL

#' @rdname baseline
#' @aliases baseline-method
setGeneric(
  name = "signal_baseline",
  def = function(object, ...) standardGeneric("signal_baseline")
)

#' @rdname baseline
#' @aliases signal_correct-method
setGeneric(
  name = "signal_correct",
  def = function(object, ...) standardGeneric("signal_correct")
)

#' @rdname baseline
#' @aliases baseline_snip-method
setGeneric(
  name = "baseline_snip",
  def = function(object, ...) standardGeneric("baseline_snip")
)

#' @rdname baseline
#' @aliases baseline_rubberband-method
setGeneric(
  name = "baseline_rubberband",
  def = function(object, ...) standardGeneric("baseline_rubberband")
)

#' @rdname baseline
#' @aliases baseline_linear-method
setGeneric(
  name = "baseline_linear",
  def = function(object, ...) standardGeneric("baseline_linear")
)

# @rdname baseline
# @aliases baseline_rollingball-method
# setGeneric(
#   name = "baseline_rollingball",
#   def = function(object, ...) standardGeneric("baseline_rollingball")
# )

# @rdname baseline
# @aliases baseline_peakfilling-method
# setGeneric(
#   name = "baseline_peakfilling",
#   def = function(object, ...) standardGeneric("baseline_peakfilling")
# )

# Dose rate ====================================================================
#' Dose Rate Estimation
#'
#' \code{dose_fit} builds a calibration curve for gamma dose rate estimation.
#'
#' \code{dose_predict} predicts in situ gamma dose rate.
#' @param object A [GammaSpectra-class] or [CalibrationCurve-class] object.
#' @param background A [GammaSpectrum-class] object of a length-two [`numeric`]
#'  vector giving the background noise integration value and error,
#'  respectively.
#' @param doses A [`matrix`] or [`data.frame`] TODO.
#' @param range_Ni,range_NiEi A length-two [`numeric`] vector giving the energy
#'  range to integrate within (in keV).
#' @param details A [`list`] of length-one vector specifying additional
#'  informations about the instrument for which the curve is built.
#' @param spectrum An optional [GammaSpectrum-class] or [GammaSpectra-class]
#'  object in which to look for variables with which to predict. If omitted,
#'  the fitted values are used.
#' @param sigma A [`numeric`] value giving TODO.
#' @param epsilon A [`numeric`] value giving an extra error term
#'  introduced by the calibration of the energy scale of the spectrum.
#' @param ... Currently not used.
#' @details
#'  To estimate the gamma dose rate, one of the calibration curves distributed
#'  with this package can be used. These built-in curves are in use in several
#'  luminescence dating laboratories and can be used to replicate published
#'  results. As these curves are instrument specific, the user may have to build
#'  its own curve.
#'
#'  The construction of a calibration curve requires a set of reference spectra
#'  for which the gamma dose rate is known and a background noise measurement.
#'  First, each reference spectrum is integrated over a given interval, then
#'  normalized to active time and corrected for background noise. The dose rate
#'  is finally modelled by the integrated signal value used as a linear
#'  predictor (York *et al.*, 2004).
#'
#'  See `vignette(doserate)` for a reproducible example.
#' @return
#'  * `dose_fit()` returns a [CalibrationCurve-class] object.
#'  * `dose_predict()` returns a [`data.frame`] with the following columns:
#'  \describe{
#'   \item{`name`}{([`character`]) the name of the spectra.}
#'   \item{`*_signal`}{([`numeric`]) the integrated signal value (according to
#'   the value of `threshold`; see [signal_integrate()]).}
#'   \item{`*_error`}{([`numeric`]) the integrated signal error value
#'   (according to the value of `threshold`; see [signal_integrate()]).}
#'   \item{`gamma_signal`}{([`numeric`]) the predicted gamma dose rate.}
#'   \item{`gamma_error`}{([`numeric`]) the predicted gamma dose rate error.}
#'  }
#' @seealso [signal_integrate()]
#' @references
#'  Mercier, N. & Falguères, C. (2007). Field Gamma Dose-Rate Measurement with
#'  a NaI(Tl) Detector: Re-Evaluation of the "Threshold" Technique.
#'  *Ancient TL*, 25(1), p. 1-4.
#'
#'  York, D., Evensen, N. M., Martínez, M. L. & De Basabe Delgado, J. (2004).
#'  Unified Equations for the Slope, Intercept, and Standard Errors of the Best
#'  Straight Line. *American Journal of Physics*, 72(3), p. 367-75.
#'  \doi{10.1119/1.1632486}.
#' @example inst/examples/ex-doserate.R
#' @author N. Frerebeau
#' @docType methods
#' @family dose rate
#' @name doserate
#' @rdname doserate
NULL

#' @rdname doserate
#' @aliases dose_fit-method
setGeneric(
  name = "dose_fit",
  def = function(object, background, doses, ...) standardGeneric("dose_fit")
)

#' @rdname doserate
#' @aliases dose_predict-method
setGeneric(
  name = "dose_predict",
  def = function(object, spectrum, ...) standardGeneric("dose_predict")
)

# Integrate ====================================================================
#' Signal Integration
#'
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class] object.
#' @param background A [GammaSpectrum-class] object.
#' @param range A length-two [`numeric`] vector giving the energy range to
#'  integrate within (in keV).
#' @param energy A [`logical`] scalar: TODO?
#' @param simplify A [`logical`] scalar: should the result be simplified to a
#'  [`matrix`]? The default value, `FALSE`, returns a [`list`].
#' @param ... Currently not used.
#' @details
#'  It assumes that each spectrum has an energy scale.
#' @return
#'  If `simplify` is `FALSE` (the default) returns a [`list`] of numeric vectors
#'  (the signal value and its error), else returns a [`matrix`].
#' @references
#'  Guérin, G. & Mercier, M. (2011). Determining Gamma Dose Rates by Field Gamma
#'  Spectroscopy in Sedimentary Media: Results of Monte Carlo Simulations.
#'  *Radiation Measurements*, 46(2), p. 190-195.
#'  \doi{10.1016/j.radmeas.2010.10.003}.
#'
#'  Mercier, N. & Falguères, C. (2007). Field Gamma Dose-Rate Measurement with
#'  a NaI(Tl) Detector: Re-Evaluation of the "Threshold" Technique.
#'  *Ancient TL*, 25(1), p. 1-4.
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname integrate
#' @aliases signal_integrate-method
setGeneric(
  name = "signal_integrate",
  def = function(object, background, ...) standardGeneric("signal_integrate")
)

# Peaks ========================================================================
#' Find Peaks
#'
#' Finds local maxima in sequential data.
#' @param object A [GammaSpectrum-class] object.
#' @param method A [`character`] string specifying the method to be used for
#'  background noise estimation (see below).
#' @param SNR An [`integer`] giving the signal-to-noise-ratio for peak detection
#'  (see below).
#' @param span An [`integer`] giving the half window size (in number of
#'  channels). If `NULL`, 5\% of the number of channels is used as the half
#'  window size.
#' @param ... Extra parameters to be passed to internal methods.
#' @details
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#'
#'  The following methods are available for noise estimation:
#'  \describe{
#'   \item{`MAD`}{Median Absolute Deviation.}
#'  }
#' @return A [PeakPosition-class] object.
#' @example inst/examples/ex-peaks.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname peaks_find
#' @aliases peaks_find-method
setGeneric(
  name = "peaks_find",
  def = function(object, ...) standardGeneric("peaks_find")
)

#' Search Peaks
#'
#' Search the maxima in sequential data around a given value.
#' @param object A [GammaSpectrum-class] object.
#' @param index A vector giving the expected peak position.
#'  If `index` is a [`numeric`] vector, peaks are searched by energy (`index` is
#'  assumed to be expressed in keV). If `index` is an [`integer`] vector, peaks
#'  are searched by channel.
#' @param span A [`numeric`] value giving the half window size for searching.
#'  If `index` is a [`numeric`] vector, `span` is expressed in keV.
#'  If `index` is an [`integer`] vector, `span` is expressed in channel.
#' @param tolerance A [`numeric`] value giving the threshold above which a
#'  warning/error is raised.
#' @param ... Currently not used.
#' @return A [PeakPosition-class] object.
#' @example inst/examples/ex-peaks.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname peaks_search
#' @aliases peaks_search-method
setGeneric(
  name = "peaks_search",
  def = function(object, index, ...) standardGeneric("peaks_search")
)

# Plot =========================================================================
#' Plot
#'
#' @param x,y Objects to be plotted.
#' @param xaxis,yaxis A [`character`] string specifying the data to be plotted
#'  along each axis. It must be one of "`energy`" or "`channel`" (`x` axis) and
#'  "`counts`" or "`rate`" (`y` axis). Any unambiguous substring can be given.
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the spectrum that are drawn.
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  spectrum be drawn?
#' @param nrow A [`character`] string specifying the number of rows.
#'  It must be one of "`fixed`" or "`auto`". Any unambiguous substring can be
#'  given. Only used if `facet` is `TRUE`.
#' @param split A [`logical`] scalar: should.
#' @param span An [`integer`] giving the half window size (in number of
#'  channels). Only used if `split` is `TRUE`.
#' @param error_ellipse A [`logical`] scalar: should error ellipses be plotted?
#' @param error_bar A [`logical`] scalar: should error bars be plotted?
#' @param level length-one [`numeric`] vector giving the the probability cutoff
#'  for the error ellipses.
#' @param n A length-one [`numeric`] vector giving the resolution of the error
#'  ellipses.
#' @param energy A [`logical`] scalar: TODO.
#' @param ... Currently not used.
#' @return
#'  A [ggplot2::ggplot] object.
#' @seealso [IsoplotR::ellipse()], [IsoplotR::isochron()]
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot
#' @name plot
#' @rdname plot
#' @aliases plot-method
if (!isGeneric("plot")) {
  setGeneric(
    name = "plot",
    def = function(x, y, ...) standardGeneric("plot")
  )
}

# Read/write data ==============================================================
#' Data Input
#'
#' Reads a gamma ray spectrum file.
#' @param file A [`character`] string giving the path of files to be imported.
#' @param extensions A [`character`] vector specifying the possible
#'  file extensions. It must be one or more of "`cnf`", "`tka`".
#' @param ... Extra parameters to be passed to [rxylib::read_xyData()].
#' @note
#'  Only supports Canberra CNF and TKA files.
#' @return
#'  A [GammaSpectra-class] object if more than one spectrum are imported
#'  at once, else a [GammaSpectrum-class] object.
#' @seealso [rxylib::read_xyData()]
#' @example inst/examples/ex-read.R
#' @author N. Frerebeau
#' @docType methods
#' @family IO
#' @rdname read
#' @aliases read-method
setGeneric(
  name = "read",
  def = function(file, ...) standardGeneric("read")
)

# Signal processing ============================================================
## Slice -----------------------------------------------------------------------
#' Choose channels by Position
#'
#' Choose channels by position.
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class]
#'  object.
#' @param ... [`integer`] values giving the channels of the spectrum to be
#'  kept/dropped (see below). Numeric values are coerced to integer as by
#'  [as.integer()] (and hence truncated towards zero).
#' @details
#'  Either positive values to keep, or negative values to drop, should be
#'  provided. The values provided must be either all positive or all negative.
#'
#'  If no value is provided, an attempt is made to define the number
#'  of channels to skip at the beginning of the spectrum. This drops all
#'  channels before the highest count maximum. This is intended to deal with the
#'  artefact produced by the rapid growth of random background noise towards low
#'  energies.
#' @return
#'  A [GammaSpectrum-class] or [GammaSpectra-class] object.
#' @author N. Frerebeau
#' @example inst/examples/ex-slice.R
#' @docType methods
#' @family signal processing
#' @rdname signal_slice
#' @aliases signal_slice-method
setGeneric(
  name = "signal_slice",
  def = function(object, ...) standardGeneric("signal_slice")
)

## Split -----------------------------------------------------------------------
#' Split
#'
#' @param object A [GammaSpectrum-class] object.
#' @param groups A a [`factor`] in the sense that `as.factor(groups)` defines
#'  the grouping (see [`split`]).
#' @param ... Currently not used.
#' @return
#'  A [GammaSpectra-class] object.
#' @author N. Frerebeau
#' @example inst/examples/ex-slice.R
#' @docType methods
#' @family signal processing
#' @rdname signal_split
#' @aliases signal_split-method
setGeneric(
  name = "signal_split",
  def = function(object, ...) standardGeneric("signal_split")
)

## Smooth ----------------------------------------------------------------------
#' Smooth
#'
#' Smoothes intensities.
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class]
#'  object.
#' @param method A [`character`] string specifying the smoothing method to be
#'  used. It must be one of "`unweighted`" (default), "`weighted`" or
#'  "`savitzky`" (see details). Any unambiguous substring can be given.
#' @param m An odd [`integer`] giving the number of adjacent points to be used.
#' @param p An [`integer`] giving the polynomial degree. Only used if `method`
#'  is "`savitzky`".
#' @param ... Extra parameters to be passed to further methods.
#' @details
#'  The following smoothing methods are available:
#'  \describe{
#'   \item{`rectangular`}{Unweighted sliding-average or rectangular smooth.
#'   It replaces each point in the signal with the average of \eqn{m} adjacent
#'   points.}
#'   \item{`triangular`}{Weighted sliding-average or triangular smooth.
#'   It replaces each point in the signal with the weighted mean of \eqn{m}
#'   adjacent points.}
#'   \item{`savitzky`}{Savitzky-Golay filter. This method is based on the
#'   least-squares fitting of polynomials to segments of \eqn{m} adjacent
#'   points.}
#'  }
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, progressively smaller smooths are used at
#'  the ends of the spectrum if `method` is `unweighted` or `weighted`. If the
#'  Savitzky-Golay filter is used, the original \eqn{(m - 1) / 2} points at the
#'  ends of the spectrum are preserved.
#' @return
#'  A [GammaSpectrum-class] or [GammaSpectra-class] object.
#' @references
#'  Gorry, P. A. (1990). General Least-Squares Smoothing and Differentiation by
#'  the Convolution (Savitzky-Golay) Method. *Analytical Chemistry*, 62(6),
#'  p. 570-573. \doi{10.1021/ac00205a007}.
#'
#'  Savitzky, A. & Golay, M. J. E. (1964). Smoothing and Differentiation of
#'  Data by Simplified Least Squares Procedures. *Analytical Chemistry*,
#'  36(8), p. 1627-1639. \doi{10.1021/ac60214a047}.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family signal processing
#' @name smooth
#' @rdname smooth
NULL

#' @rdname smooth
#' @aliases signal_smooth-method
setGeneric(
  name = "signal_smooth",
  def = function(object, ...) standardGeneric("signal_smooth")
)

#' @rdname smooth
#' @aliases smooth_rectangular-method
setGeneric(
  name = "smooth_rectangular",
  def = function(object, ...) standardGeneric("smooth_rectangular")
)

#' @rdname smooth
#' @aliases smooth_triangular-method
setGeneric(
  name = "smooth_triangular",
  def = function(object, ...) standardGeneric("smooth_triangular")
)

#' @rdname smooth
#' @aliases smooth_savitzky-method
setGeneric(
  name = "smooth_savitzky",
  def = function(object, ...) standardGeneric("smooth_savitzky")
)

## Stabilize -------------------------------------------------------------------
#' Transform Intensities
#'
#' @param object A [GammaSpectrum-class] object.
#' @param f A [`function`] that takes a numeric vector as
#'  argument and returns a numeric vector.
#' @param ... Extra arguments to be passed to `f`.
#' @details
#'  The stabilization step aims at improving the identification of peaks with a
#'  low signal-to-noise ratio. This particularly targets higher energy peaks.
#' @return A [GammaSpectrum-class] or [GammaSpectra-class] object with
#'  transformed intensities.
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing
#' @rdname stabilize
#' @aliases signal_stabilize-method
setGeneric(
  name = "signal_stabilize",
  def = function(object, ...) standardGeneric("signal_stabilize")
)

# Summarize ====================================================================
#' Summarize
#'
#' @param object A [GammaSpectrum-class] or [GammaSpectra-class] object.
#' @param ... Currently not used.
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @example inst/examples/ex-summarise.R
#' @docType methods
#' @family IO
#' @rdname summarise
#' @aliases summarise-method
setGeneric(
  name = "summarise",
  def = function(object, ...) standardGeneric("summarise")
)
