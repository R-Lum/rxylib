# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# GammaSpectrum ================================================================
setValidity(
  Class = "GammaSpectrum",
  method = function(object) {
    hash <- object@hash
    name <- object@name
    date <- object@date
    instrument <- object@instrument
    file_format <- object@file_format
    channel <- object@channel
    energy <- object@energy
    count <- object@count
    rate <- object@rate
    live_time <- object@live_time
    real_time <- object@real_time
    calibration <- object@calibration
    message <- c()

    length_hash <- length(hash)
    if (length_hash != 0) {
      nchar_hash <- nchar(hash)
      if (nchar_hash != 32) {
        message <- c(
          message,
          sprintf(
            "%s must be a 32-character string (not %d) of hexadecimal digits.",
            sQuote("hash"), nchar_hash)
        )
      }
    }
    length_name <- length(name)
    if (length_name > 1) {
      message <- c(
        message,
        sprintf("%s must be a character vector of length one, not %d.",
                sQuote("name"), length_name)
      )
    }
    length_instrument <- length(instrument)
    if (length_instrument > 1) {
      message <- c(
        message,
        sprintf("%s must be a character vector of length one, not %d.",
                sQuote("instrument"), length_instrument)
      )
    }
    length_file_format <- length(file_format)
    if (length_file_format > 1) {
      message <- c(
        message,
        sprintf("%s must be a character vector of length one, not %d.",
                sQuote("file_format"), length_file_format)
      )
    }
    length_live_time <- length(live_time)
    if (length_live_time > 1) {
      message <- c(
        message,
        sprintf("%s must be character vector of length one, not %d.",
                sQuote("live_time"), length_live_time)
      )
      if (!isPositive(live_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("live_time"))
        )
      }
    }
    length_real_time <- length(real_time)
    if (length_real_time > 1) {
      message <- c(
        message,
        sprintf(
          "%s must be a character vector of length one, not %d.",
          sQuote("real_time"), length_real_time)
      )
      if (!isPositive(real_time, strict = TRUE)) {
        message <- c(
          message,
          sprintf("%s must be a strictly positive number.", sQuote("real_time"))
        )
      }
    }
    # TODO: check calibration
    if (length(channel) != 0) {
      if (!all(isPositive(channel, strict = TRUE))) {
        message <- c(
          message,
          sprintf("%s must be a vector of strictly positive numbers.",
                  sQuote("channel"))
        )
      }
    }
    # TODO: check counts (baseline may produces negative count)
    # TODO: check rate (baseline may produces negative count)
    if (length(channel) != 0) {
      if (!isEqual(lengths(list(channel, count)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("channel"), length(channel),
            sQuote("count"), length(count)
          )
        )
      }
    }
    if (length(rate) != 0) {
      if (!isEqual(lengths(list(count, rate)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("count"), length(count),
            sQuote("rate"), length(rate)
          )
        )
      }
    }
    if (length(energy) != 0) {
      if (!isEqual(lengths(list(energy, count)))) {
        message <- c(
          message,
          sprintf(
            "%s (%d) and %s (%d) must have the same length.",
            sQuote("energy"), length(energy),
            sQuote("count"), length(count)
          )
        )
      }
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n  * "))
    } else {
      return(TRUE)
    }
  }
)

# GammaSpectra =================================================================
setValidity(
  Class = "GammaSpectra",
  method = function(object) {
    data <- object@.Data
    message <- c()

    if (length(data) != 0) {
      data_class <- unlist(lapply(X = data, FUN = is, class2 = "GammaSpectrum"))
      if (!all(data_class)) {
        message <- c(message,
                     "All elements must be of class `GammaSpectrum`.")
      }
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n* "), call. = FALSE)
    } else {
      return(TRUE)
    }
  }
)

# CalibrationCurve =============================================================
# setValidity(
#   Class = "CalibrationCurve",
#   method = function(object) {
#     details <- object@details
#     Ni_model <- object@Ni_model
#     Ni_noise <- object@Ni_noise
#     Ni_range <- object@Ni_range
#     NiEi_model <- object@NiEi_model
#     NiEi_noise <- object@NiEi_noise
#     NiEi_range <- object@NiEi_range
#     data <- object@data
#     message <- c()
#
#     if (length(details) != 0) {
#       if (!("date" %in% names(details))) {
#         message <- c(
#           message,
#           "Slot `details` is a list, but does not have a component `date`."
#         )
#       } else {
#         if (!methods::is(details$date, "POSIXct")) {
#           message <- c(message, "Slot `date` must be a `POSIXct` object.")
#         }
#       }
#       if (any(lengths(details) != 1)) {
#         message <- c(
#           message,
#           "Slot `details` is a list, but some components are not of length 1."
#         )
#       }
#     }
#     length_Ni_noise <- length(Ni_noise)
#     if (length_Ni_noise != 0) {
#       if (!isPositive(Ni_noise, strict = FALSE)) {
#         message <- c(message,
#                      "Slot `Ni_noise` must be a vector of positive numbers.")
#       }
#       if (length_Ni_noise != 2) {
#         message <- c(
#           message,
#           sprintf("Slot `Ni_noise` must be a numeric vector of length 2, not %d.",
#                   length_Ni_noise)
#         )
#       }
#     }
#     length_NiEi_noise <- length(NiEi_noise)
#     if (length_NiEi_noise != 0) {
#       if (!isPositive(NiEi_noise, strict = FALSE)) {
#         message <- c(message,
#                      "Slot `NiEi_noise` must be a vector of positive numbers.")
#       }
#       if (length_NiEi_noise != 2) {
#         message <- c(
#           message,
#           sprintf("Slot `NiEi_noise` must be a numeric vector of length 2, not %d.",
#                   length_NiEi_noise)
#         )
#       }
#     }
#     # length_integration <- length(integration)
#     # if (length_integration != 0) {
#     #   if (!isPositive(integration, strict = FALSE)) {
#     #     message <- c(message,
#     #                  "Slot `integration` must be a vector of positive numbers.")
#     #   }
#     #   if (length_integration != 2) {
#     #     message <- c(
#     #       message,
#     #       sprintf("Slot `integration` must be a numeric vector of length 2, not %d.",
#     #               length_integration)
#     #     )
#     #   }
#     # }
#     # if (length(data) != 0) {
#     #   if (!is.numeric(as.matrix(data[, -1]))) {
#     #     message <- c(message, "Slot `data` must contain numeric values.")
#     #   }
#     #   ncol_data <- ncol(data)
#     #   if (ncol_data != 10) {
#     #     message <- c(
#     #       message,
#     #       sprintf("Slot `data` must be a 10 (not %d) columns data.frame.",
#     #               ncol_data)
#     #     )
#     #   }
#     # }
#
#     if (length(message) != 0) {
#       stop("* ", paste0(message, collapse = "\n* "), call. = FALSE)
#     } else {
#       return(TRUE)
#     }
#   }
# )

# PeakPosition =================================================================
setValidity(
  Class = "PeakPosition",
  method = function(object) {
    hash <- object@hash
    noise_method <- object@noise_method
    threshold <- object@noise_threshold
    window <- object@window
    channel <- object@channel
    energy_observed <- object@energy_observed
    energy_expected <- object@energy_expected
    message <- c()

    length_hash <- length(hash)
    if (length_hash != 1 || (length_hash == 1 && nchar(hash) != 32)) {
      message <- c(message, "Slot `hash` must be a 32-character string.")
    }
    length_method <- length(noise_method)
    if (length_method > 1) {
      message <- c(
        message,
        sprintf("Slot `noise_method` must be a character vector of length 1, not %d.",
                length_method)
      )
    }
    length_noise <- length(threshold)
    if (length_noise > 1) {
      message <- c(
        message,
        sprintf(
          "Slot `noise_threshold` must be a numeric vector of length 1, not %d.",
          length_noise
        )
      )
    } else if (!is.na(threshold) && !isPositive(threshold, strict = FALSE)) {
      message <- c(message,
                   "Slot `noise_threshold` must be a positive number.")
    }
    length_window <- length(window)
    if (length_window > 1) {
      message <- c(
        message,
        sprintf("Slot `window` must be an integer vector of length 1, not %d.",
                length_window)
      )
    } else if (!is.na(window) && !isPositive(window, strict = FALSE)) {
      message <- c(
        message,
        sprintf("Slot `window` must be a strictly positive integer, not %d.",
                window)
      )
    }
    if (length(channel) != length(energy_observed)) {
      message <- c(
        message,
        "Slots `channel` and `energy_observed` must have the same length."
      )
    }
    if (length(channel) != length(energy_expected)) {
      message <- c(
        message,
        "Slots `channel` and `energy_expected` must have the same length."
      )
    }

    if (length(message) != 0) {
      stop("* ", paste0(message, collapse = "\n* "), call. = FALSE)
    } else {
      return(TRUE)
    }
  }
)
