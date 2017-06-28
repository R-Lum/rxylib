#' Import XY-Data into R
#'
#' @description
#' Provides access to the 'xylib' C library for to import xy
#' data from powder diffraction, spectroscopy and other experimental methods.
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
#' @author Sebastian Kreutzer
#'
#' @keywords package
#' @import utils
#' @importFrom Rcpp evalCpp
#' @useDynLib rxylib, .registration=TRUE
NULL
