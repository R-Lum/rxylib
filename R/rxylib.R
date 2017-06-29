#' Import XY-Data into R
#'
#' @description
#' Provides access to the 'xylib' C library for to import xy
#' data from powder diffraction, spectroscopy and other experimental methods, like gamma-ray spectrometry.
#'
#' \tabular{ll}{ Package: \tab rxylib\cr Type: \tab Package\cr Version:
#' \tab 0.1.0 \cr Date: \tab 2017-06-30 \cr License: \tab GPL-3 | LGPL-2.1 (for the C++ library 'xylib') \cr }
#'
#' @details
#'
#' Currently the package uses `xylib` in version 1.6 (unreleased).
#'
#' Supported data formats
#'
#' @name rxylib-package
#' @aliases rxylib-package rxylib
#' @docType package
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), RLum Team (family support), Marcin Wojdyr (maintainer and author of the C++ library `xylib`), Peng Zhang (author of the C++ library `xylib`)
#'
#' @keywords package
#' @import utils
#' @importFrom Rcpp evalCpp
#' @useDynLib rxylib, .registration=TRUE
NULL
