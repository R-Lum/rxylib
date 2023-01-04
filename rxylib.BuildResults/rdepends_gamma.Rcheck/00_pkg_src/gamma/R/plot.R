# PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "missing"),
  definition = function(x, xaxis = c("channel", "energy"),
                        yaxis = c("count", "rate"), ...) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)

    # Get data
    calib <- has_energy(x)
    spc <- as.data.frame(x)
    if (xaxis == "energy" && anyNA(spc$energy)) {
      xaxis <- "channel"
      warning("The energy scale is missing, displaying channels instead.",
              call. = FALSE)
    }
    xlabel <- switch(xaxis, channel = "Channel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, count = "Counts", rate = "Count rate [1/s]")

    # Plot
    ggplot(spc, aes(x = .data[[xaxis]], y = .data[[yaxis]])) +
      labs(x = xlabel, y = ylabel) +
      geom_path()
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,Baseline-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "Baseline"),
  definition = function(x, y, xaxis = c("channel", "energy"),
                        yaxis = c("count", "rate"), ...) {
    set_names(y) <- "Baseline"
    spc <- .GammaSpectra(list(x, y))
    plot(spc, xaxis = xaxis, yaxis = yaxis, select = NULL, facet = FALSE)
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectra,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectra", y = "missing"),
  definition = function(x, xaxis = c("channel", "energy"),
                        yaxis = c("count", "rate"),
                        select = NULL, facet = FALSE,
                        nrow = c("fixed", "auto")) {
    # Validation
    xaxis <- match.arg(xaxis, several.ok = FALSE)
    yaxis <- match.arg(yaxis, several.ok = FALSE)
    nrow <- match.arg(nrow, several.ok = FALSE)

    if (is.null(select))
      select <- seq_len(length(x))
    if (is.numeric(select))
      select <- as.integer(select)

    # Subset data and build a long data frame
    spc <- as_long(x[select])
    n <- nlevels(as.factor(spc$name))

    if (xaxis == "energy" & anyNA(spc$energy)) {
      xaxis <- "channel"
      warning("The energy scale is missing for one or more spectra.\n",
              "Displaying channels instead.", call. = FALSE)
    }
    xlabel <- switch(xaxis, channel = "Channel", energy = "Energy [keV]")
    ylabel <- switch(yaxis, count = "Counts", rate = "Count rate [1/s]")

    facet <- if (n == 1) FALSE else facet
    if (facet) {
      scales <- switch(nrow, fixed = "free_y", auto = "free")
      nrow <- switch(nrow, fixed = n, auto = NULL)
      facet <- facet_wrap(vars(.data$name), nrow = nrow, scales = scales)
      aes_plot <- aes(
        x = .data[[xaxis]],
        y = .data[[yaxis]],
        group = .data$name
      )
    } else {
      facet <- NULL
      aes_plot <- aes(
        x = .data[[xaxis]],
        y = .data[[yaxis]],
        group = .data$name,
        colour = .data$name
      )
    }
    ggplot(data = spc, mapping = aes_plot) +
      geom_path() +
      labs(x = xlabel, y = ylabel, colour = "Name") +
      facet
  }
)

#' @export
#' @rdname plot
#' @aliases plot,GammaSpectrum,PeakPosition-method
setMethod(
  f = "plot",
  signature = signature(x = "GammaSpectrum", y = "PeakPosition"),
  definition = function(x, y, split = FALSE, span = 25) {
    # Validation
    if (x@hash != y@hash)
      stop("`x` and `y` do not match.", call. = FALSE)

    # Get data
    pks <- as.data.frame(y)
    pks$name <- paste(get_names(x), pks$channel, sep = "_")
    peak_channel <- pks$channel
    peak_energy <- pks$energy_observed

    index_energy <- !is.na(peak_energy)
    if (any(index_energy)) {
      sec_axis <- sec_axis(
        trans = ~.,
        name = "Energy [keV]",
        breaks = peak_channel[index_energy],
        labels = round(peak_energy[index_energy], 0)
      )
    } else {
      sec_axis <- waiver()
    }
    peak_legend <- scale_x_continuous(
      breaks = peak_channel,
      labels = peak_channel,
      sec.axis = sec_axis
    )

    if (split) {
      grp <- rep(NA, length(x))
      spc_channel <- get_channels(x)
      for (i in seq_along(peak_channel)) {
        chan <- peak_channel[[i]]
        j <- spc_channel >= chan - span & spc_channel < chan + span
        grp[j] <- as.character(chan)
      }
      x <- signal_split(x, groups = grp)
    }

    plot(x, facet = split, nrow = "auto") +
      geom_vline(data = pks, aes(xintercept = .data$channel),
                 linetype = 3, colour = "red") +
      peak_legend
  }
)

#' @export
#' @rdname plot
#' @aliases plot,CalibrationCurve,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "CalibrationCurve", y = "missing"),
  definition = function(x, error_ellipse = TRUE, error_bar = FALSE,
                        energy = FALSE, level = 0.95, n = 50, ...) {
    # Validation
    k <- ifelse(energy, "NiEi", "Ni")

    # Get data
    model <- x[[k]]
    data <- model[["data"]]

    # Curve
    segment_x <- range(data$signal_value)
    segment_y <- model[["slope"]][[1L]] * segment_x + model[["intercept"]][[1L]]
    segment <- rbind.data.frame(c(segment_x, segment_y))
    names(segment) <- c("x", "xmin", "y", "ymin")

    # Errors
    ggellipse <- ggbar <- NULL
    ndata <- nrow(data)
    if (error_ellipse) { # Ellipse
      ell <- vector(mode = "list", length = ndata)
      for (i in seq_len(ndata)) {
        mtx <- cor2cov2(data$signal_error[i], data$gamma_error[i], 0)
        ell[[i]] <- IsoplotR::ellipse(x = data$signal_value[i],
                                      y = data$gamma_dose[i],
                                      covmat = mtx, alpha = 1 - level, n = n)
      }
      ell <- do.call(rbind.data.frame, ell)
      ell$group <- rep(seq_len(ndata), each = n)
      ggellipse <- ggplot2::geom_path(
        mapping = aes(x = .data$x, y = .data$y, group = .data$group),
        data = ell, colour = "red", inherit.aes = FALSE
      )
    }
    if (error_bar) { # Crossbar
      ggbar <- list(
        geom_errorbar(
          mapping = aes(ymin = .data$gamma_dose - .data$gamma_error,
                        ymax = .data$gamma_dose + .data$gamma_error),
          width = 0,
          colour = "red"),
        geom_errorbarh(
          mapping = aes(xmin = .data$signal_value - .data$signal_error,
                        xmax = .data$signal_value + .data$signal_error),
          height = 0,
          colour = "red")
      )
    }

    ggplot(data = data) +
      aes(x = .data$signal_value, y = .data$gamma_dose, label = .data$names) +
      geom_segment(
        data = segment,
        mapping = aes(x = .data$x, xend = .data$xmin,
                      y = .data$y, yend = .data$ymin),
        inherit.aes = FALSE
      ) +
      geom_point() +
      ggellipse + ggbar +
      labs(x = sprintf("Signal [%s]", k), y = "Dose rate [\u03BCGy/y]")
  }
)

# COPY FROM NON EXPORTED ISOPLOTR
cor2cov2 <- function(sX, sY, rXY){
  covmat <- matrix(0, 2, 2)
  covmat[1, 1] <- sX^2
  covmat[2, 2] <- sY^2
  covmat[1, 2] <- rXY * sX * sY
  covmat[2, 1] <- covmat[1, 2]
  covmat
}
