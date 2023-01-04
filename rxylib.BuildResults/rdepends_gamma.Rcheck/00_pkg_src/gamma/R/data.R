# ============================================================ Internal datasets
#' Nuclear Decay Data
#'
#' An internal dataset containing the decay data for the following isotopes:
#' 232-Th, 235-U, 238-U and 40-K.
#' @format
#'  A [`data.frame`] with the following columns (absolute errors):
#'  \describe{
#'    \item{decay_chain}{}
#'    \item{isotope}{}
#'    \item{occurrence}{}
#'    \item{occurrence_error}{}
#'    \item{post_radon}{}
#'    \item{half_life}{}
#'    \item{half_life_error}{}
#'    \item{energy}{Energy in keV.}
#'    \item{energy_error}{}
#'    \item{gamma_intensity}{}
#'    \item{gamma_intensity_error}{}
#'    \item{counts_chain}{}
#'    \item{counts_chain_error}{}
#'  }
#' @source
#'  Nuclides And Isotopes : Chart of the Nuclides.
#' @name decay
#' @rdname decay
#' @keywords datasets internal
# ".decay"
NULL

#' Lanthanum Decay Data
#'
#' An internal dataset containing the decay data for the Lanthanum isotopes.
#' @format
#'  A [`data.frame`] with the following columns (absolute errors):
#'  \describe{
#'    \item{decay_chain}{}
#'    \item{isotope}{}
#'    \item{occurrence}{}
#'    \item{occurrence_error}{}
#'    \item{post_radon}{}
#'    \item{half_life}{}
#'    \item{half_life_error}{}
#'    \item{energy}{Energy in keV.}
#'    \item{energy_error}{}
#'    \item{gamma_intensity}{}
#'    \item{gamma_intensity_error}{}
#'  }
#' @source
#'  Nuclides And Isotopes : Chart of the Nuclides.
#' @name decay_La
#' @rdname decay_La
#' @keywords datasets internal
# ".decay_La"
NULL

# ==============================================================================
#' Clermont Reference Data
#'
#' @usage data("clermont")
#' @format TODO
#' @source
#'  Guérin, G., Mercier, N. & Adamiec, G. (2011). Dose-Rate Conversion Factors:
#'  Update. *Ancient TL*, 29(1), p. 5-8.
#'
#'  Miallier, D., Guérin, G., Mercier, N., Pilleyre, T. & Sanzelle, S.
#'  (2009). The Clermont Radiometric Reference Rocks: A Convenient Tool
#'  for Dosimetric Purposes. *Ancient TL*, 27(2), p. 37-44.
#' @family datasets
#' @keywords datasets
"clermont"

#' CEREGE Calibration Curve (NaI)
#'
#' @usage data(AIX_NaI_1)
#' @format
#' An object of class [CalibrationCurve-class].
#' \tabular{ll}{
#'  \strong{Laboratory} \tab CEREGE \cr
#'  \strong{Instrument} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector} \tab NaI \cr
#'  \strong{Authors} \tab CEREGE Luminescence Team
#' }
#' @examples
#' ## Load the curve
#' utils::data(AIX_NaI_1, package = "gamma")
#' plot(AIX_NaI_1)
#' @family datasets
#' @keywords datasets
"AIX_NaI_1"

#' CRP2A Calibration Curve (LaBr)
#'
#' @usage data(BDX_LaBr_1)
#' @format
#' An object of class [CalibrationCurve-class].
#' \tabular{ll}{
#'  \strong{Laboratory} \tab IRAMAT-CRP2A (UMR 5060) \cr
#'  \strong{Instrument} \tab Canberra Inspector 1000 \cr
#'  \strong{Detector} \tab LaBr \cr
#'  \strong{Authors} \tab CRP2A Luminescence Team
#' }
#' @examples
#' ## Load the curve
#' utils::data(BDX_LaBr_1, package = "gamma")
#' plot(BDX_LaBr_1)
#' @family datasets
#' @keywords datasets
"BDX_LaBr_1"
