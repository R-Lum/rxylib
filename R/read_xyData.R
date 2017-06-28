#' Import xy-Data for Supported Formats into R
#'
#' The function provides an access to the underlying `xylib` to import data for supported file formats
#' into R. Usually just the file path is needed. The function automatically recognises allowed
#' formats.See [rxylib-package] for supported formats.
#'
#' @param file [character] (**required**): path and file to be imported. The argument accepts an `URL`.
#'
#' @param verbose [logical] (*with default*): enables/disables verbose mode
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @return The functions returns a [list] of matrices.
#'
#' @keywords IO
#'
#' @examples
#'
#' \dontrun{
#' ##load example dataset
#' file <- system.file("extdata/ExampleSpectrum.CNF", package = "rxylib")
#' results <- read_xyData(file)
#'
#' ##plot spectrum
#' plot(results[[1]],
#'  type = "l",
#'  log = "y",
#'  xlab = "Energy [keV]",
#'  ylab = "Counts",
#'  main = "Thorite - 1800 s")
#'
#' mtext(side = 3, "Canberra Inspector 1000, 3 x 3 NaI probe")
#'
#' }
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

      ##check whether the file as an URL
      if(grepl(pattern = "http", x = file, fixed = TRUE)){
        if(verbose){
          cat("[read_xyData()] URL detected, checking connection ... ")
        }

        ##check URL
        if(!httr::http_error(file)){
          if(verbose) cat("OK")

          ##dowload file
          file_link <- paste0(tempfile("read_xyData"), ".", rev(strsplit(file, split = ".", fixed = TRUE)[[1]])[1])
          download.file(file, destfile = file_link, quiet = ifelse(verbose, FALSE, TRUE), mode = "wb")
          file <- file_link

        }else{
          cat("FAILED")
          con <- NULL
          stop("[read_xyData()] File could not be downloaded!", call. = FALSE)

        }

      }else{
        stop("[read_xyData()] File does not exists!", call. = FALSE)

      }

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
        cat("\n[read_xyData()] >> Plain TXT-file detected\n")

      }

    }else if(any(stringr::str_detect(df_supported$exts, pattern = ext))){
      format_name <- df_supported[which(stringr::str_detect(df_supported$exts, pattern = ext)), "name"]
      if(verbose){
        cat("\n[read_xyData()] >> File of type ")
        cat(df_supported[which(stringr::str_detect(df_supported$exts, pattern = ext)), "desc"])
        cat(" detected\n")

      }

    }else{
      stop(paste0("[read_xyData()] File extension '*.", ext, "' is not supported!"), call. = FALSE)

    }


  # Import data ---------------------------------------------------------------------------------
  return(read_data(path = file, format_name = format_name))

}
