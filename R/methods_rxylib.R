##################################################################################
##                      METHODS FOR S3 GENERICS                                 ##
##################################################################################

#' methods_ryxlib
#'
#' S3-methods support by the package `rxylib`. Listed functions can be passed directly into
#' S3 generics (e.g., [plot], [print]) without reshaping the data.
#'
#' @param x (**required**): input opject
#'
#' @param ... further arguments that can be passed to the method
#'
#' @md
#' @name methods_rxylib
NULL

# ####################################################################################################
# methods for generic: print()
# ##################################################################################################
#' @rdname methods_rxylib
#' @method print rxylib
#' @export
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
      cat("\n  .. $metadata_block \t", ifelse(nrow(x$dataset[[i]]$metadata_block) == 0, FALSE, TRUE))

  }

}

# ####################################################################################################
# methods for generic: plot()
# ##################################################################################################
#' @rdname methods_rxylib
#' @method plot rxylib
#'
#' @param block [numeric] (with default): select block for plotting, e.g. `c(1:2)`.
#'
#' @md
#' @export
plot.rxylib <- function(x, block = NULL, ...) {

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

  ##check if block number is set
  if(!is.null(block)){
    if(!block%in%(1:length(x$dataset))){
      try(stop("[plot.rxylib] Block index not valid, return NULL!", call. = FALSE))
      return(NULL)

    }

  }else{
    block <- 1:length(x$dataset)

  }

  ##return what is inside
  for(i in block){
    args <-
    do.call(
      what = "plot",
      args = c(list(x = x$dataset[[i]]$data_block), plot_settings))

  }

  invisible(NULL)
}
