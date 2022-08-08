#' Import xy-Data for Supported Formats into R
#'
#'The function provides access to the underlying `xylib` to import data for supported file formats
#'into R. In most cases, only the file path is needed with further arguments to import the data.
#'The function automatically recognises allowed formats. See [rxylib-package] for supported formats.
#'
#' @param file [character] (**required**): path and file to be imported. The argument accepts an `URL`.
#'
#' @param options [character] (with default): set format options (see [rxylib-package])
#'
#' @param verbose [logical] (*with default*): enables/disables verbose mode
#'
#' @param metaData [logical] (*with default*): enables/disables the export of metadata
#'
#' @section Function version: 0.3.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France), Johannes Friedrich,
#' University of Bayreuth (Germany)
#'
#' @return The functions returns a [list] of matrices.
#'
#' @keywords IO
#'
#' @examples
#'
#' ##load example dataset
#' file <- system.file("extdata/ExampleSpectrum.CNF", package = "rxylib")
#' results <- read_xyData(file)
#' results
#'
#' ##plot xy-spectrum
#' plot(results,
#'  type = "l",
#'  xlab = "Energy [keV]",
#'  ylab = "Counts",
#'  main = "Thorite - 1800 s")
#'
#' mtext(side = 3, "Canberra Inspector 1000, 3 x 3 NaI probe")
#'
#' ##plot contour for TL-spectrum
#' ##imported from an XSYG-file
#' spectrum <- read_xyData(system.file("extdata/TLSpectrum.xsyg", package = "rxylib"))
#' contour(
#'  x = spectrum$dataset[[1]]$data_block[,1],
#'  y = 1:ncol(spectrum$dataset[[1]]$data_block[,-1]),
#'  z = spectrum$dataset[[1]]$data_block[,-1],
#'  xlab = "Wavelength [nm]",
#'  ylab = "#Channel",
#'  main = "TL Spectrum")
#'
#'
#' @md
#' @export
read_xyData <- function(
  file,
  options = "",
  verbose = TRUE,
  metaData = TRUE
){

  # Integrity tests -----------------------------------------------------------------------------
  # (it is safer to run them here, instead of the compiled code)

    ##check whether file exists
    if(!file.exists(file)){

      ##check whether the file as an URL
      if(grepl(pattern = "http", x = file, fixed = TRUE)){
        if(verbose){
          cat("[read_xyData()] URL detected, try download ... ")
        }

        ##set file link
        file_link <- paste0(tempfile("read_xyData"), ".", rev(strsplit(file, split = ".", fixed = TRUE)[[1]])[1])

        ##try download
        try <- try(download.file(file, destfile = file_link, quiet = ifelse(verbose, FALSE, TRUE), mode = "wb"), silent = TRUE)
        file <- file_link

        ##check and stop if necessary
        if(inherits(try, "try-error")){
          con <- NULL
          try(stop("[read_xyData()] File could not be downloaded, NULL returned!", call. = FALSE))
          return(NULL)

        }


      }else{
        try(stop("[read_xyData()] File does not exist, NULL returned!", call. = FALSE))
        return(NULL)

      }

    }

  # Set file extension  -------------------------------------------------------------------------

    ##provide full path (the underlying C++ code does not like weired paths)
    file <- paste0(dirname(file),"/",basename(file))

    ##extract file extension
    ext <- rev(strsplit(x = basename(file), split = ".", fixed = TRUE)[[1]])[1]

      ##make small letters out of it, otherwise it may not work if, for whatever reason,
      ##the filename was written in capital letters
      ext <- tolower(ext)

    ##construct data.frame of supported file formats
    df_supported <- as.data.frame(get_supportedFormats(), stringsAsFactors = FALSE)

    supported_ext <- unlist(lapply(1:length(df_supported$exts), function(x){
      strsplit(df_supported$exts[x], "\\s+")

    }))

    ##check whether the extension is in the list + txt
    if(ext %in% c(supported_ext,"txt")){
      format_name <- df_supported[grep(x = df_supported$exts, pattern = ext, fixed = TRUE), "name"]

      ##check for format length and allow auto detect by the library
      if (ext == "txt" || length(format_name) > 1) {
        format_name <- ""
        text <- "\n[read_xyData()] >> Non-obvious format, run auto detection ...\n"

      }else{
        text <- paste0("\n[read_xyData()] >> File of type ",
                  df_supported[grep(x = df_supported$exts, pattern = ext, fixed = TRUE), "desc"],
                  " detected\n")

      }

      if(verbose){
        cat(text)

      }

    }else{
      try(stop(paste0("[read_xyData()] File extension '*.", ext, "' is not supported! Return NULL!"), call. = FALSE))
      return(NULL)

    }

    # READ Data Import ----------------------------------------------------------------------------
    data <- try(read_data(path = file, format_name = format_name, options = options, metaData = metaData), silent = TRUE)

    if(inherits(data, "try-error")){
      try(stop("[read_xyData()] Data import failed. Return NULL!", call. = FALSE))
      return(NULL)

    }

    # READ Metadata -------------------------------------------------------------------------------
    if(metaData){
      dataSet_metaData <- try(get_meta_DataSet(path = file, format_name = format_name, options = options), silent = TRUE)

      if(inherits(dataSet_metaData, "try-error")){
        try(stop("[read_xyData()] Metadata extraction failed! Set to NULL!", call. = FALSE))
        dataSet_metaData <- NULL
        metaData <- FALSE

      }

    } else {
      dataSet_metaData <- NULL

    }

    # READ Block names ----------------------------------------------------------------------------
    #extract blockNames
    block_names <- try(get_block_names(path = file, format_name = format_name, options = options), silent = TRUE)

      #if it fails ... just return NA (silently it is not important)
      if(!inherits(block_names, "try-error")){
        ##set block names (each list element)
        names(data) <- block_names

      }


  # return data ---------------------------------------------------------------------------------
  output <- list(
    dataset = data,
    metadata = dataSet_metaData)

  # set format name attribute
  attr(output, "format_name") <- df_supported[grep(x = df_supported$exts, pattern = ext, fixed = TRUE), "desc"]

  ##set class
  class(output) <- "rxylib"

  ##return
  return(output)

}
