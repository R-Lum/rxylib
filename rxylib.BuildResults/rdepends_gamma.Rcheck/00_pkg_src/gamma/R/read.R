# READ DATA FILES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname read
#' @aliases read,character-method
setMethod(
  f = "read",
  signature = signature(file = "character"),
  definition = function(file, extensions = c("cnf", "tka"), ...) {
    # Validation
    extensions <- match.arg(extensions, several.ok = TRUE)
    extensions <- c(extensions, toupper(extensions))

    # If input is a directory and not a single file
    # Then, look for all files with allowed extensions
    if (!all(utils::file_test("-f", file))) {
      file_list <- tools::list_files_with_exts(file, exts = extensions)
      if (length(file_list) == 0)
        stop("No spectrum files were fund.", call. = FALSE)
      file <- as.list(file_list)
    }

    # Read files
    spc <- lapply(X = file, FUN = function(x, y, ...) {
      extension <- tolower(tools::file_ext(x))
      switch(
        extension,
        cnf = readCanberraCNF(file = x, ...),
        tka = readCanberraTKA(file = x, ...)
      )
    }, ...)

    if (length(spc) > 1) {
      # Return a GammaSpectra object
      methods::new("GammaSpectra", spc)
    } else {
      # Return a GammaSpectrum
      spc[[1]]
    }
  }
)

#' Read Canberra CNF file
#'
#' @param file A [`character`] string giving the path and file to be imported.
#' @param ... Extra parameters passed to [rxylib::read_xyData()].
#' @return
#'  An object of class [GammaSpectrum-class].
#' @keywords internal
#' @noRd
readCanberraCNF <- function(file, ...) {
  # Read file
  spc_xy <- rxylib::read_xyData(file = file, ..., verbose = FALSE)
  # Get and check file format
  file_format <- attr(spc_xy, "format_name")

  # Get metadata
  spc_meta <- spc_xy$dataset[[1]]$metadata_block
  date <- as.POSIXct(spc_meta[1, 2], format = c("%a, %Y-%m-%d %H:%M:%S"))
  live_time <- as.numeric(spc_meta[5, 2])
  real_time <- as.numeric(spc_meta[6, 2])

  # Get data
  spc_data <- as.data.frame(spc_xy$dataset[[1]]$data_block)
  # Add a column to store the channel number
  spc_data[["channel"]] <- as.integer(seq_len(nrow(spc_data)))
  colnames(spc_data) <- c("energy", "count", "channel")

  # Get instrument name (remove the last word)
  instrument_name <- gsub("\\s*\\w*$", "", names(spc_xy$dataset))

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  .GammaSpectrum(
    hash = hash,
    name = tools::file_path_sans_ext(basename(file)),
    date = date,
    instrument = instrument_name,
    file_format = "CNF",
    channel = as.integer(spc_data$channel),
    energy = spc_data$energy,
    count = as.integer(spc_data$count),
    live_time = live_time,
    real_time = real_time
  )
}

#' Read Canberra TKA file
#'
#' @param file A [`character`] string giving the path and file to be imported.
#' @param ... Currently not used.
#' @return
#'  An object of class [GammaSpectrum-class].
#' @keywords internal
#' @noRd
readCanberraTKA <- function(file, ...) {
  # Read file
  spc_xy <- utils::read.table(file = file)

  # Get metadata
  live_time <- as.numeric(spc_xy[1, 1])
  real_time <- as.numeric(spc_xy[2, 1])
  instrument_name <- "unknown"

  # Get data
  spc_data <- data.frame(count = as.numeric(spc_xy[, 1]))
  # Remove the first two value (live time and real time)
  spc_data[c(1, 2), 1] <- c(0, 0)
  # Add a column to store the channel number
  spc_data[["channel"]] <- as.integer(seq_len(nrow(spc_data)))
  colnames(spc_data) <- c("counts", "channel")

  # Compute 32-bytes MD5 hash
  hash <- as.character(tools::md5sum(file))

  .GammaSpectrum(
    hash = hash,
    name = tools::file_path_sans_ext(basename(file)),
    instrument = instrument_name,
    file_format = "TKA",
    channel = as.integer(spc_data$channel),
    count = as.integer(spc_data$count),
    live_time = live_time,
    real_time = real_time
  )
}
