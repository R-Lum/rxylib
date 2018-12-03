#' Convert xy-data to TKA
#'
#' Convert data to the Toolkit file format (TKA) as exported by, e.g., by the software
#' Canberra Genie 2000.
#'
#' **Supported formats**
#'
#' - Canberra CNF
#' - further formats on request ...
#'
#'
#' @param object [rxylib] (**required**): xy data as imported by the function [read_xyData]. Optional
#' a file supported by the `rxylib`-package can be provided as input. Arguments can be provided as [list].
#'
#' @param file [character] (optional): optional file path or file name for the output to be written.
#' If only a path is provided the output file name is derived from the input file name. Argument
#' can be provided as [list].
#'
#' @param overwrite [logical] (with default): force overwriting of existing files if `TRUE`.
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
#' ##convert CNF data (no export to file system)
#' convert_xy2TKA(
#'  object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"))
#'
#' \dontrun{
#' ##export as file
#'
#' ##create temporary filepath
#' ##(for usage replace by own path)
#' temp_file <- tempfile(pattern = "output", fileext = ".TKA")
#'
#' ##convert and write to file system
#' convert_xy2TKA(
#'  object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"),
#'  file = temp_file)
#'
#' }
#'
#' @md
#' @export
convert_xy2TKA <- function(
  object,
  file = NULL,
  overwrite = FALSE
){


  # self call -----------------------------------------------------------------------------------
  if(class(object) == "list" || (class(object) == "character" && length(object) > 1)){

    ##convert to list in either case
    if(class(object) == "character")
      object <- as.list(object)

    ##expand
    if(class(file) == "list"){
      file <- rep_len(file,length.out = length(object))

    }else{
      file <- rep_len(as.list(file),length.out = length(object))

    }

    ##run the function
    output <- unlist(lapply(1:length(object), function(x){
      convert_xy2TKA(object[[x]], file[[x]])

    }), recursive = FALSE)

    ##return
    return(output)
  }


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
    stop(paste0("[write_xy2TKA()] Sorry, no support for ",attr(object, "format_name"), " implemented!"),
         call. = FALSE)


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

      ##check whether files exists
      if(file.exists(file) & !overwrite){
        message(paste0("[convert_xy2TKA()] File ", file, " already exists, skipped!"))

      }else{
        ##write
        write.table(x = output[[i]], file = file, append = FALSE, row.names = FALSE, col.names = FALSE)

      }

    }

  }

}
