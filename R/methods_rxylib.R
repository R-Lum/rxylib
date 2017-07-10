##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################

#' methods_ryxlib
#'
#' @name methods_rxylib
NULL

# ####################################################################################################
# methods for generic: print()
# ##################################################################################################
#' @rdname methods_rxylib
#' @method print rxylib
#' @noRd
print.rxylib <- function(x, ...) {
  cat("\n[rxylib-object]\n")
  cat("\n  Imported Format:\t", attr(x, "format_name"))
  cat("\n  Imported datasets: \t", length(x$dataset))


}

