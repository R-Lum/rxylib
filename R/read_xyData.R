#' Import xy-Data for Supported Formats into R
#'
#' The function provides an access to the underlying `xylib` to import data for supported file formats
#' into R
#'
#' @param file [character] (**required**): file to be imported
#'
#' @param verbose [logical] (*with default*): enables/disables verbose mode
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @keywords IO
#'
#' @examples
#'
#' ## still missing
#'
#' @md
#' @export
read_xyData <- function(
  file,
  verbose = TRUE
){

  # Integrity tests -----------------------------------------------------------------------------
  # (it is safer to run them here, instead of the compiled code)

    ##check whether file exists
    if(!file.exists(file)){
      stop("[read_xyData()] File does not exists!", call. = FALSE)
    }


  # Set file extension  -------------------------------------------------------------------------

    ##provide full path (the underlying C++ code does not like weired paths
    file <- paste0(dirname(file),"/",basename(file))

    ##extract file extension
    ext <- rev(strsplit(x = basename(file), split = ".", fixed = TRUE)[[1]])[1]

      ##make small letters out of it, otherwise it may not work if, for whatever reason,
      ##the filename was written in capital letters
      ext <- tolower(ext)

    ##construct data.frame of supported file formats
    df_supported <- as.data.frame(get_supportedFormats(), stringsAsFactors = FALSE)

    ##check whether the extension is in the list
    if(ext == "txt"){
      format_name <- ""
      if(verbose){
        cat("\n[read_xtData()] >> Plain TXT-file detected\n")

      }

    }else if(any(stringr::str_detect(df_supported$exts, pattern = ext))){
      format_name <- df_supported[which(stringr::str_detect(df_supported$exts, pattern = ext)), "name"]
      if(verbose){
        cat("\n[read_xtData()] >> File of type ")
        cat(df_supported[which(stringr::str_detect(df_supported$exts, pattern = ext)), "desc"])
        cat(" detected\n")

      }

    }else{
      stop(paste0("[read_xyData()] File extension '*.", ext, "' is not supported!"), call. = FALSE)

    }


  # Import data ---------------------------------------------------------------------------------
  return(read_data(path = file, format_name = format_name))

}
