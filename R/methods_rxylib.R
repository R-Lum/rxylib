##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################

#TODO: add metadata information once the data.frame was fixed (no factors!)

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
  cat("\n  Imported format:\t", attr(x, "format_name"))
  cat("\n  Imported blocks: \t", length(x$dataset))
  cat("\n  Dataset has metadata:\t", ifelse(nrow(x$metadata) == 0, FALSE, TRUE))
  cat("\n")

  ##return what is inside
  for(i in 1:length(x$dataset)){
      cat("\n  << block", i,">>\n")
      cat("  .. $data_block", "\t" ,
          is(x$dataset[[i]]$data_block)[1], ":",paste(dim(x$dataset[[i]]$data_block), collapse = " x "))
      cat("\n  .. $metadata_block \t", is(x$dataset[[i]]$metadata_block)[1])

  }

}

# ####################################################################################################
# methods for generic: plot()
# ##################################################################################################
#' @rdname methods_rxylib
#' @method plot rxylib
#' @noRd
plot.rxylib <- function(x, ...) {

  ##preset plot settings
  plot_settings.default <- list(
    xlab = "x [a.u.]",
    ylab = "y [a.u.]"

  )

  ##remove values from list which are in the default, otherwise we get an error
  call <- names(as.list(sys.call()))
  call <- call[call!=""]
  plot_settings.default[names(call)] <- NULL

  ##modify list on request
  plot_settings <- modifyList(x = plot_settings.default, val = list(...))

  ##return what is inside
  for(i in 1:length(x$dataset)){
    do.call(what = "plot", args = c(list(x = x$dataset[[i]]$data_block[,1], y = x$dataset[[i]]$data_block[,2]), plot_settings))

  }

}
