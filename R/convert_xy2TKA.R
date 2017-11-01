#' Export xy-data to TKA
#'
#' Export data to the Toolkit file format (TKA) as exported by, e.g., by the software
#' Canberra Genie 2000.
#'
#' **Supported formats**
#'
#' - Canberra CNF
#' - further formats on request ...
#'
#'
#' @param object [rxylib] (**required**): xy data as imported by the function [read_xyData]. Optional
#' a file supported by the `rxylib`-package can be provided as input.
#'
#' @param file [character] (optional): optional file path or file name for the output to be written.
#' If only a path is provided the output file name is derived from the input file name
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @return Returns a [list] of [matrix] objects or an output TKA-file.
#'
#' @keywords IO
#'
#' @examples
#'
#' ##convert CNF data
#' convert_xy2TKA(
#'  object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"))
#'
#' \dontrun{
#' ##export as file
#' convert_xy2TKA(
#'  object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"),
#'  file = "~/Desktop/")
#'
#' }
#'
#' @md
#' @export
convert_xy2TKA <- function(
  object,
  file = NULL
){


  # Set objects ---------------------------------------------------------------------------------
  file_name <- "output"


  # Integrity tests -----------------------------------------------------------------------------
  ##The functions allows to inputs
  ##(1) object can be a file connection ... here a character
  if(class(object) == "character"){
    file_name <- basename(object)
    object <- read_xyData(object, verbose = FALSE)

    if(is.null(object))
      return(object)

  }

  ##(2) object is just an object
  if(class(object) != "rxylib")
    stop("[write_xy2TKA()] Argument 'object' requires an S3-object of class 'rxylib'")

  # Convert -------------------------------------------------------------------------------------

  ## The conversion depends on the input file format - add further file format support here
  ## Canberra CNF
  if(attr(object, "format_name") == "Canberra CNF"){
    output <- lapply(object[[1]], function(x){
       matrix(
         c(trunc(as.numeric(x[["metadata_block"]][[2]][5:6])), x[["data_block"]][,2]), ncol = 1)

    })

  }else {
    stop(paste0("[write_xy2TKA()] Sorry, no support for ",attr(object, "format_name"), " implemented!"), call. = FALSE)


  }

  # Export --------------------------------------------------------------------------------------
  # Consider two cases: (1) output is a list, (2) output is a file
  if(is.null(file)){
    return(output)

  }else{
    for(i in 1:length(output)){
      ##check if the output is a directory ... in this case we take the input or
      ##the default filename
      if(dir.exists(file)){
        file <- paste0(file,"/",file_name)

      }else{
        ##remove the tailing .TKA
        file <- sub(pattern = ".TKA", replacement = "", x = file, ignore.case = TRUE)

      }

      ##account for the case of multiple bocks
        ##remove all dots in the name
        file <- sub(pattern = ".", replacement = "_", x = file, fixed = TRUE)

        ##add a number if needed
        if(length(output) > 1){
          file <- paste0(file,"_",i,".TKA")

        }else{
          file <- paste0(file,".TKA")

        }

      ##write
      write.table(x = output[[i]], file = file, append = FALSE, row.names = FALSE, col.names = FALSE)

    }

  }

}
