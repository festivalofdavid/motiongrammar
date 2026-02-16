# segment name: filt_sma ---

#' Simple Moving Average filter
#' @param x Numeric vector to filter.
#' @param window Integer; number of points in the moving window.
#' @return Numeric vector of the same length (with NAs at edges).
#' @keywords internal
filt_sma <- function(x, window = 5) {
  weights <- rep(1 / window, window)
  as.numeric(stats::filter(x, filter = weights, sides = 2))
}

# segment name: filt_ema ---

#' Exponential Moving Average filter
#' @param x Numeric vector to filter.
#' @param alpha Numeric; smoothing factor between 0 and 1.
#' @return Numeric vector of the same length.
#' @keywords internal
filt_ema <- function(x, alpha = 0.3) {
  as.numeric(stats::filter(x * alpha, filter = 1 - alpha, method = 'recursive'))
}

# segment name: filt_savgol ---

#' Savitzky-Golay filter
#' @param x Numeric vector to filter.
#' @param window Integer; odd number for the filter window length.
#' @param poly_order Integer; polynomial order for the fit.
#' @return Numeric vector of the same length.
#' @keywords internal
filt_savgol <- function(x, window = 5, poly_order = 3) {
  if (!requireNamespace('signal', quietly = TRUE)) {
    stop('Package "signal" is required for Savitzky-Golay filtering. Please install it.')
  }
  as.numeric(signal::sgolayfilt(x, p = poly_order, n = window))
}

# segment name: filt_residual_analysis ---

#' Residual analysis for cutoff frequency optimisation
#'
#' Applies a filter at each cutoff in a range and computes RMS of residuals.
#' Based on the Winter (2009) residual analysis approach.
#'
#' @param x Numeric vector (raw signal).
#' @param method Character; filter method to use.
#' @param cutoff_range Numeric length-2; min and max cutoff to test.
#' @param n_steps Integer; number of cutoff values to evaluate.
#' @param ... Additional arguments passed to the filter (e.g. n, type).
#' @return A data.frame with columns \code{cutoff} and \code{rms_residual}.
#' @keywords internal
filt_residual_analysis <- function(x, method = 'butterworth',
                                   cutoff_range = c(0.01, 0.5),
                                   n_steps = 50, ...) {
  cutoffs <- seq(cutoff_range[1], cutoff_range[2], length.out = n_steps)
  dots <- list(...)

  # Track the actual parameter value used for each step
  param_vals <- numeric(n_steps)

  rms_vals <- vapply(seq_along(cutoffs), function(i) {
    co <- cutoffs[i]
    filtered <- tryCatch({
      if (method == 'butterworth') {
        if (!requireNamespace('signal', quietly = TRUE)) stop('signal required')
        n_ord <- dots$n %||% 2
        type  <- dots$type %||% 'low'
        bf <- signal::butter(n = n_ord, W = co, type = type)
        param_vals[i] <<- co
        as.numeric(signal::filtfilt(bf, x))
      } else if (method == 'sma') {
        w <- max(3, round(1 / co))
        param_vals[i] <<- w
        filt_sma(x, window = w)
      } else if (method == 'ema') {
        param_vals[i] <<- co
        filt_ema(x, alpha = co)
      } else if (method == 'savgol') {
        w <- max(5, round(1 / co))
        if (w %% 2 == 0) w <- w + 1
        param_vals[i] <<- w
        filt_savgol(x, window = w, poly_order = dots$poly_order %||% 3)
      } else {
        stop('Unknown method')
      }
    }, error = function(e) {
      param_vals[i] <<- co
      rep(NA_real_, length(x))
    })

    residuals <- x - filtered
    sqrt(mean(residuals^2, na.rm = TRUE))
  }, numeric(1))

  data.frame(parameter = param_vals, rms_residual = rms_vals)
}

# segment name: filt_psd ---

#' Power Spectral Density estimation
#'
#' Computes PSD using \code{stats::spectrum()} to help identify
#' where signal content ends and noise begins.
#'
#' @param x Numeric vector (raw signal).
#' @param hz Numeric; sampling frequency in Hz.
#' @return A data.frame with columns \code{frequency} and \code{power}.
#' @keywords internal
filt_psd <- function(x, hz) {
  sp <- stats::spectrum(x, plot = FALSE)
  data.frame(
    frequency = sp$freq * hz,
    power     = sp$spec
  )
}

# segment name: filtrate ---

#' Filter Motion Trace Noise on xyz coordinates
#'
#' @description
#' Applies digital filters to x, y, and z coordinates to remove GPS jitter.
#'
#' @param .data A motion_trace object.
#' @param method Character; the filtering algorithm. One of
#'   \code{'butterworth'}, \code{'sma'}, \code{'ema'}, or \code{'savgol'}.
#' @param n Filter order (Butterworth only).
#' @param cutoff Filter frequency (Butterworth only).
#' @param type Filter type, e.g. \code{'low'} or \code{'high'} (Butterworth only).
#' @param window Integer; window size for SMA or Savitzky-Golay filters.
#' @param alpha Numeric; smoothing factor for EMA (0-1).
#' @param poly_order Integer; polynomial order for Savitzky-Golay filter.
#'
#' @return A filtered \code{motion_trace} object.
#' @export
filtrate <- function(.data,
                     method = 'butterworth',
                     n = 2,
                     cutoff = 0.1,
                     type = 'low',
                     window = 5,
                     alpha = 0.3,
                     poly_order = 2){

  # Validation: Ensure coordinates exist and have real data
  if (!all(c('x', 'y') %in% names(.data))) {
    stop('Coordinates missing. Run coordinate() before filtrate().')
  }

  n_valid_x <- sum(!is.na(.data$x))
  n_valid_y <- sum(!is.na(.data$y))
  if (n_valid_x < 4 || n_valid_y < 4) {
    stop('Not enough valid (non-NA) coordinate data to filter. ',
         'x has ', n_valid_x, ' and y has ', n_valid_y, ' valid rows.')
  }

  has_z <- 'z' %in% names(.data)

  # Helper: apply a filter function only to non-NA values,
  # preserving NAs in their original positions
  safe_filter <- function(vals, filter_fn, ...) {
    out <- rep(NA_real_, length(vals))
    valid <- !is.na(vals)
    if (sum(valid) < 4) return(out)
    out[valid] <- filter_fn(vals[valid], ...)
    out
  }

  #  Filter Selection Logic ---
  if (method == 'butterworth') {

    # Make sure we have signal package installed
    if (!requireNamespace('signal', quietly = TRUE)){
      stop('Package "signal" is required for Butterworth filtering. Please install it.')
    }

    # Run butterworth according to our specifications
    bf <- signal::butter(n = n, W = cutoff, type = type)

    bw_filter <- function(v, ...) as.numeric(signal::filtfilt(bf, v))

    output <- .data |>
      dplyr::mutate(
        f_x = safe_filter(x, bw_filter),
        f_y = safe_filter(y, bw_filter),
        f_z = if (has_z) safe_filter(z, bw_filter) else 0
      )

  } else if (method == 'sma') {

    output <- .data |>
      dplyr::mutate(
        f_x = safe_filter(x, filt_sma, window = window),
        f_y = safe_filter(y, filt_sma, window = window),
        f_z = if (has_z) safe_filter(z, filt_sma, window = window) else 0
      )

  } else if (method == 'ema') {

    output <- .data |>
      dplyr::mutate(
        f_x = safe_filter(x, filt_ema, alpha = alpha),
        f_y = safe_filter(y, filt_ema, alpha = alpha),
        f_z = if (has_z) safe_filter(z, filt_ema, alpha = alpha) else 0
      )

  } else if (method == 'savgol') {

    output <- .data |>
      dplyr::mutate(
        f_x = safe_filter(x, filt_savgol, window = window, poly_order = poly_order),
        f_y = safe_filter(y, filt_savgol, window = window, poly_order = poly_order),
        f_z = if (has_z) safe_filter(z, filt_savgol, window = window, poly_order = poly_order) else 0
      )

  } else {
    stop('Method "', method, '" not yet implemented.')
  }

  # Preserve class and update metadata
  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }

  # Build parameter list for this filter pass
  filter_params <- switch(method,
    butterworth = list(n = n, cutoff = cutoff, type = type),
    sma         = list(window = window),
    ema         = list(alpha = alpha),
    savgol      = list(window = window, poly_order = poly_order)
  )

  output <- filt_quality_log(output, method, filter_params)

  return(output)
}

# segment name: find_inflection ---

#' Find inflection point of a residual curve
#'
#' Uses a smoothing spline to estimate the second derivative and locates the
#' point of maximum absolute curvature (the "elbow" of the residual curve).
#'
#' @param x Numeric vector; parameter values (x-axis).
#' @param y Numeric vector; RMS residual values (y-axis).
#' @return Numeric scalar; the parameter value at the inflection point.
#' @keywords internal
.est_inflection <- function(x, y) {
  # Remove NAs and duplicate x values
  valid <- !is.na(y) & !duplicated(x)
  x <- x[valid]
  y <- y[valid]

  if (length(x) < 4) return(x[which.min(y)])

  # Fit smoothing spline and get second derivative
  sp <- stats::smooth.spline(x, y, spar = 0.6)
  d2 <- stats::predict(sp, x, deriv = 2)

  # Inflection = maximum absolute second derivative
  x[which.max(abs(d2$y))]
}

# segment name: filt_quality_log ---

#' Quality log for the filtrate step
#'
#' Appends a filtrate entry to the quality attribute. Supports multiple
#' filter passes (e.g. Savitzky-Golay followed by SMA) by storing each
#' pass as a numbered entry in a list.
#'
#' @param output A motion_trace object (post-filtering).
#' @param method Character; the filter method that was applied.
#' @param params Named list; the parameters used for this filter pass.
#' @return The motion_trace object with updated quality attribute.
#' @keywords internal
filt_quality_log <- function(output, method, params) {

  qual <- attr(output, 'quality')
  if (is.null(qual)) qual <- list()
  meta <- attr(output, 'metadata')

  # Determine which packages this method depends on
  filt_deps <- switch(method,
    butterworth = c('signal', 'dplyr'),
    sma         = c('stats', 'dplyr'),
    ema         = c('stats', 'dplyr'),
    savgol      = c('signal', 'dplyr'),
    'dplyr'
  )
  filt_deps <- unique(filt_deps)

  dep_versions <- vapply(filt_deps, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "unknown")
  }, character(1))

  # Compute summary stats on the filtered columns
  axes_filtered <- intersect(c('f_x', 'f_y', 'f_z'), names(output))
  na_counts <- vapply(axes_filtered, function(col) {
    sum(is.na(output[[col]]))
  }, integer(1))

  # Build this filter pass entry
  pass_entry <- list(
    method     = method,
    timestamp  = Sys.time(),
    parameters = params,
    axes_filtered   = axes_filtered,
    na_introduced   = na_counts,
    total_rows      = nrow(output),
    dependencies    = setNames(dep_versions, filt_deps)
  )

  # Support multiple filter passes: store as an ordered list
  if (is.null(qual$filtrate)) {
    qual$filtrate <- list(
      step   = "filtrate",
      passes = list(pass_entry)
    )
  } else {
    qual$filtrate$passes <- c(qual$filtrate$passes, list(pass_entry))
  }

  # Update metadata: store full filter chain (not just the last one)
  meta$filters_applied <- vapply(qual$filtrate$passes, function(p) p$method, character(1))
  attr(output, 'metadata') <- meta
  attr(output, 'quality') <- qual
  output
}

# segment name: filtrate_cutoff ---

#' Visualise optimal cutoff frequency for filtering
#'
#' Runs residual analysis and/or power spectral density estimation on a
#' chosen axis and returns a ggplot2 scree/elbow-style plot to help
#' identify the optimal cutoff frequency.
#'
#' @param .data A motion_trace object with x, y (and optionally z) columns.
#' @param method Character; filter method to evaluate (default \code{'butterworth'}).
#' @param cutoff_range Numeric length-2; min and max cutoff to sweep.
#' @param n_steps Integer; number of cutoff values to test.
#' @param axis Character; which axis to analyse (\code{'x'}, \code{'y'}, or \code{'z'}).
#' @param plot_type Character; \code{'residual'}, \code{'psd'}, or \code{'both'}.
#' @param ... Additional arguments passed to the underlying filter
#'   (e.g. \code{n}, \code{type} for Butterworth).
#'
#' @return A \code{ggplot} object.
#' @export
filtrate_cutoff <- function(.data,
                              method = 'butterworth',
                              cutoff_range = c(0.01, 0.5),
                              n_steps = 50,
                              axis = 'x',
                              plot_type = 'both',
                              ...) {

  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package "ggplot2" is required for filtrate_cutoff(). Please install it.')
  }

  if (!all(c('x', 'y') %in% names(.data))) {
    stop('Coordinates missing. Run coordinate() before filtrate_cutoff().')
  }

  if (!(axis %in% names(.data))) {
    stop('Axis "', axis, '" not found in data.')
  }

  signal_vec <- .data[[axis]]
  signal_vec <- signal_vec[!is.na(signal_vec)]

  # Determine sampling rate from metadata or timestamps
  meta <- attr(.data, 'metadata')
  hz <- meta$hz %||% meta$sample_rate %||% 10

  # Determine x-axis label based on method
  param_label <- switch(method,
    butterworth = 'Cutoff Frequency',
    sma         = 'Window Size',
    ema         = 'Alpha (Smoothing Factor)',
    savgol      = 'Window Size',
    'Parameter'
  )

  plots <- list()

  # Residual analysis
  if (plot_type %in% c('residual', 'both')) {
    res_df <- filt_residual_analysis(signal_vec,
                                     method = method,
                                     cutoff_range = cutoff_range,
                                     n_steps = n_steps,
                                     ...)

    # Estimate ideal cutoff via inflection point (max absolute second derivative)
    ideal <- .est_inflection(res_df$parameter, res_df$rms_residual)

    plots$residual <- ggplot2::ggplot(res_df, ggplot2::aes(x = parameter, y = rms_residual)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_vline(xintercept = ideal, linetype = 'dashed', colour = 'red') +
      ggplot2::annotate('text', x = ideal, y = max(res_df$rms_residual, na.rm = TRUE),
                        label = paste0('Suggested: ', round(ideal, 4)),
                        hjust = -0.1, vjust = 1, colour = 'red', size = 3.5) +
      ggplot2::labs(
        title = 'Residual Analysis',
        x = param_label,
        y = 'RMS Residual'
      ) +
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = element_blank())
  }

  # Power spectral density
  if (plot_type %in% c('psd', 'both')) {
    psd_df <- filt_psd(signal_vec,
                       hz = hz)

    plots$psd <- ggplot2::ggplot(psd_df,
                                 ggplot2::aes(x = frequency,
                                                      y = power)) +
      ggplot2::geom_line() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(
        title = 'Power Spectral Density',
        x = 'Frequency (Hz)',
        y = 'Power'
      ) +
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = element_blank())
  }

  # Return single or faceted plot
  if (plot_type == 'both') {
    if (!requireNamespace('patchwork', quietly = TRUE)) {
      message('Install "patchwork" for side-by-side plots. Returning residual plot only.')
      return(plots$residual)
    }
    return(plots$residual + plots$psd + patchwork::plot_layout(ncol = 2))
  }

  plots[[1]]
}
