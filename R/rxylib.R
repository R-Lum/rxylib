#' Import XY-Data into R
#'
#' @description
#' Provides access to the 'xylib' C library for to import xy
#' data from powder diffraction, spectroscopy and other experimental methods.
#'
#' \tabular{ll}{ Package: \tab rxylib\cr Type: \tab Package\cr Version:
#' \tab 0.1.0 \cr Date: \tab 2017-06-30 \cr License: \tab GPL-3\cr }
#'
#' @details
#'
#' Currently the package uses `xylib` version 1.6 (unreleased).
#'
#' Supported data formats
#'
#' @name rxylib-package
#' @aliases rxylib-package rxylib
#' @docType package
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Marcin Wojdyr (maintainer and author
#' library `xylib`), Peng Zhang (author library `xylib`)
#'
#' @keywords package
#' @import utils
#' @importFrom Rcpp evalCpp
#' @useDynLib rxylib, .registration=TRUE
NULL
