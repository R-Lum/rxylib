#' Import XY-Data into R
#'
#' @description
#' Provides access to the 'xylib' C library for to import xy
#' data from powder diffraction, spectroscopy and other experimental methods, like gamma-ray spectrometry.
#'
#' \tabular{ll}{ Package: \tab rxylib\cr Type: \tab Package\cr Version:
#' \tab 0.2.0 \cr Date: \tab 2017-XX-XX \cr License: \tab GPL-3 | LGPL-2.1 (for the C++ library 'xylib') \cr }
#'
#' @details
#'
#' Supported data formats
#'
#' @name rxylib-package
#' @aliases rxylib-package rxylib
#' @docType package
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Johannes Friedrich (University of Bayreuth, Germany), RLum Team (family support), Marcin Wojdyr (maintainer and author of the C++ library `xylib`), Peng Zhang (author of the C++ library `xylib`)
#'
#' @keywords package
#' @import utils methods
#' @importFrom graphics plot
#' @importFrom Rcpp evalCpp
#' @useDynLib rxylib, .registration=TRUE
NULL
