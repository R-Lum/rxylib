#' @title Import XY-Data into R
#'
#' \if{html}{
#' \figure{rxylib.svg}{options: width="50"}\cr
#' }
#'
#' @description
#' Provides access to the 'xylib' C++ library for to import xy
#' data from powder diffraction, spectroscopy and other experimental methods,
#' like gamma-ray spectrometry.\cr
#'
#' License: GPL-3 | LGPL-2.1 (for the C++ library 'xylib')
#'
#' @details
#'
#' **Funding**
#'
#' Between 2017-2019, the work of Sebastian Kreutzer as maintainer of the package was supported
#' by LabEx LaScArBx (ANR - n. ANR-10-LABX-52).
#'
#' From 01/2020-04/2022, Sebastian Kreutzer received funding from the European Union’s Horizon 2020
#' research and innovation programme under the Marie Skłodowska-Curie grant
#' agreement No 844457 (project: CREDit).
#'
#' Supported data formats:
#'
#' @name rxylib-package
#' @aliases rxylib-package rxylib
#' @author Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl-University of Heidelberg (Germany), Johannes Friedrich (University of Bayreuth, Germany), RLum Team (family support), Marcin Wojdyr (maintainer and author of the C++ library `xylib`), Peng Zhang (author of the C++ library `xylib`)
#'
#' @md
#' @keywords package
#' @import utils methods
#' @importFrom graphics plot
#' @importFrom Rcpp evalCpp
#' @useDynLib rxylib, .registration=TRUE
"_PACKAGE"
