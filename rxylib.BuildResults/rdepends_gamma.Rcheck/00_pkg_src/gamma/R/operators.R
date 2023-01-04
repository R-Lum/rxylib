# OPERATIONS
#' @include AllGenerics.R AllClasses.R
NULL

# Arith ========================================================================
#' @export
#' @rdname operator
#' @aliases Arith,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "Arith",
  signature(e1 = "GammaSpectrum", e2 = "GammaSpectrum"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2@count)
    spc <- methods::initialize(e1, count = value)
    return(spc)
  }
)

#' @export
#' @rdname operator
#' @aliases Arith,GammaSpectrum,numeric-method
setMethod(
  f = "Arith",
  signature(e1 = "GammaSpectrum", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2)
    spc <- methods::initialize(e1, count = value)
    return(spc)
  }
)

# Compare ======================================================================
#' @export
#' @rdname operator
#' @aliases Compare,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "Compare",
  signature(e1 = "GammaSpectrum", e2 = "GammaSpectrum"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2@count)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Compare,GammaSpectrum,numeric-method
setMethod(
  f = "Compare",
  signature(e1 = "GammaSpectrum", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2)
    return(value)
  }
)

# Logic ========================================================================
#' @export
#' @rdname operator
#' @aliases Logic,GammaSpectrum,GammaSpectrum-method
setMethod(
  f = "Logic",
  signature(e1 = "GammaSpectrum", e2 = "GammaSpectrum"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2@count)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,GammaSpectrum,numeric-method
setMethod(
  f = "Logic",
  signature(e1 = "GammaSpectrum", e2 = "numeric"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2)
    return(value)
  }
)

#' @export
#' @rdname operator
#' @aliases Logic,GammaSpectrum,logical-method
setMethod(
  f = "Logic",
  signature(e1 = "GammaSpectrum", e2 = "logical"),
  definition = function(e1, e2) {
    value <- methods::callGeneric(e1 = e1@count, e2 = e2)
    return(value)
  }
)

# Math =========================================================================
#' @export
#' @rdname operator
#' @aliases Math,GammaSpectrum-method
setMethod(
  f = "Math",
  signature(x = "GammaSpectrum"),
  definition = function(x) {
    methods::callGeneric(x = x@count)
  }
)


# Math2 ========================================================================
#' @export
#' @rdname operator
#' @aliases Math2,GammaSpectrum-method
setMethod(
  f = "Math2",
  signature(x = "GammaSpectrum"),
  definition = function(x, digits) {
    methods::callGeneric(x = x@count, digits = digits)
  }
)

# Summary ======================================================================
#' @export
#' @rdname operator
#' @aliases Summary,GammaSpectrum-method
setMethod(
  f = "Summary",
  signature(x = "GammaSpectrum"),
  definition = function(x, ..., na.rm = FALSE) {
    methods::callGeneric(x = x@count, ..., na.rm = na.rm)
  }
)
