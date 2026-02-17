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
#' @param cutoff_range Numeric length-2; min and max cutoff to test (Hz for
#'   Butterworth; generic parameter range for other methods).
#' @param n_steps Integer; number of cutoff values to evaluate.
#' @param hz Numeric; sampling rate in Hz. Required for Butterworth to convert
#'   cutoff from Hz to normalised frequency.
#' @param ... Additional arguments passed to the filter (e.g. n, type).
#' @return A data.frame with columns \code{parameter} and \code{rms_residual}.
#' @keywords internal
filt_residual_analysis <- function(x, method = 'butterworth',
                                   cutoff_range = c(0.01, 0.5),
                                   n_steps = 50, hz = NULL, ...) {
  cutoffs <- seq(cutoff_range[1], cutoff_range[2], length.out = n_steps)
  dots <- list(...)

  nyquist <- if (!is.null(hz)) hz / 2 else NULL

  # Track the actual parameter value used for each step
  param_vals <- numeric(n_steps)

  rms_vals <- vapply(seq_along(cutoffs), function(i) {
    co <- cutoffs[i]
    filtered <- tryCatch({
      if (method == 'butterworth') {
        if (!requireNamespace('signal', quietly = TRUE)) stop('signal required')
        if (is.null(nyquist)) stop('hz is required for Butterworth residual analysis')
        n_ord <- dots$n %||% 2
        type  <- dots$type %||% 'low'
        W <- co / nyquist
        if (W <= 0 || W >= 1) return(rep(NA_real_, length(x)))
        bf <- signal::butter(n = n_ord, W = W, type = type)
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

# segment name: resolve_target_cols ---

#' Resolve target shorthand to column names
#'
#' Maps user-facing target names to actual column names for filtering.
#'
#' @param target Character; one of \code{'coordinates'}, \code{'all_derivatives'},
#'   \code{'velocity'}, \code{'acceleration'}, \code{'angular_velocity'}, or a
#'   character vector of specific column names.
#' @param .data A data.frame to check column existence against.
#' @return A character vector of column names to filter.
#' @keywords internal
.resolve_target_cols <- function(target, .data) {
  deriv_map <- list(
    velocity         = 'velocity',
    acceleration     = 'acceleration',
    angular_velocity = 'angular_velocity'
  )

  if (length(target) == 1 && target == 'coordinates') {
    cols <- c('x', 'y')
    # Only include z if it exists and has real (non-NA) data
    if ('z' %in% names(.data) && sum(!is.na(.data[['z']])) >= 4) {
      cols <- c(cols, 'z')
    }
    return(cols)
  }

  if (length(target) == 1 && target == 'all_derivatives') {
    target <- names(deriv_map)
  }

  # Expand shorthand names
  cols <- unname(vapply(target, function(t) {
    if (t %in% names(deriv_map)) deriv_map[[t]] else t
  }, character(1)))

  missing <- setdiff(cols, names(.data))
  if (length(missing) > 0) {
    stop('Column(s) not found in data: ', paste(missing, collapse = ', '),
         '. Run derivate() before filtering derivatives.')
  }

  cols
}

# segment name: filtrate ---

#' Filter Motion Trace Noise
#'
#' @description
#' Applies digital filters to coordinates or derivative signals (velocity,
#' acceleration, angular velocity) to remove noise.
#'
#' @param .data A motion_trace object.
#' @param method Character; the filtering algorithm. One of
#'   \code{'butterworth'}, \code{'sma'}, \code{'ema'}, or \code{'savgol'}.
#' @param target Character; what to filter. One of \code{'coordinates'}
#'   (default), \code{'all_derivatives'}, \code{'velocity'},
#'   \code{'acceleration'}, \code{'angular_velocity'}, or a character vector
#'   combining any of these (e.g. \code{c('velocity', 'acceleration')}).
#' @param n Filter order (Butterworth only).
#' @param cutoff Cutoff frequency in Hz (Butterworth only). Must be less than
#'   half the sampling rate (Nyquist frequency).
#' @param type Filter type, e.g. \code{'low'} or \code{'high'} (Butterworth only).
#' @param window Integer; window size for SMA or Savitzky-Golay filters.
#' @param alpha Numeric; smoothing factor for EMA (0-1).
#' @param poly_order Integer; polynomial order for Savitzky-Golay filter.
#'
#' @return A filtered \code{motion_trace} object with \code{f_} prefixed columns.
#' @export
filtrate <- function(.data,
                     method = 'butterworth',
                     target = 'coordinates',
                     n = 2,
                     cutoff = 1,
                     type = 'low',
                     window = 5,
                     alpha = 0.3,
                     poly_order = 2){

  # Resolve which columns to filter
  target_cols <- .resolve_target_cols(target, .data)

  # Validation: enough non-NA data to filter
  for (col in target_cols) {
    n_valid <- sum(!is.na(.data[[col]]))
    if (n_valid < 4) {
      stop('Not enough valid (non-NA) data in "', col, '" to filter (',
           n_valid, ' valid rows).')
    }
  }

  # Helper: apply a filter function only to non-NA values,
  # preserving NAs in their original positions
  safe_filter <- function(vals, filter_fn, ...) {
    out <- rep(NA_real_, length(vals))
    valid <- !is.na(vals)
    if (sum(valid) < 4) return(out)
    out[valid] <- filter_fn(vals[valid], ...)
    out
  }

  # Build the filter function for the chosen method
  if (method == 'butterworth') {

    if (!requireNamespace('signal', quietly = TRUE)){
      stop('Package "signal" is required for Butterworth filtering. Please install it.')
    }

    meta <- attr(.data, 'metadata')
    hz <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 10
    nyquist <- hz / 2
    W <- cutoff / nyquist
    if (W <= 0 || W >= 1) {
      stop('Cutoff (', cutoff, ' Hz) must be between 0 and Nyquist (',
           nyquist, ' Hz) for sampling rate ', hz, ' Hz.')
    }

    bf <- signal::butter(n = n, W = W, type = type)
    filter_fn <- function(v, ...) as.numeric(signal::filtfilt(bf, v))
    filter_args <- list()

  } else if (method == 'sma') {

    filter_fn <- filt_sma
    filter_args <- list(window = window)

  } else if (method == 'ema') {

    filter_fn <- filt_ema
    filter_args <- list(alpha = alpha)

  } else if (method == 'savgol') {

    filter_fn <- filt_savgol
    filter_args <- list(window = window, poly_order = poly_order)

  } else {
    stop('Method "', method, '" not yet implemented.')
  }

  # Apply filter to each target column, creating f_ prefixed output
  # On repeated passes, filter the already-filtered column (f_) if it exists
  output <- .data
  for (col in target_cols) {
    f_col <- paste0('f_', col)
    source_col <- if (f_col %in% names(output) && sum(!is.na(output[[f_col]])) >= 4) f_col else col
    output[[f_col]] <- do.call(safe_filter,
      c(list(vals = output[[source_col]], filter_fn = filter_fn), filter_args))
  }

  # For coordinates target: ensure f_z exists even when z is absent
  if (identical(target, 'coordinates') && !('z' %in% names(.data))) {
    output[['f_z']] <- 0
  }

  # Preserve class and update metadata
  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }

  # Build parameter list for this filter pass
  filter_params <- switch(method,
    butterworth = list(n = n, cutoff_hz = cutoff, type = type),
    sma         = list(window = window),
    ema         = list(alpha = alpha),
    savgol      = list(window = window, poly_order = poly_order)
  )

  output <- filt_quality_log(output, method, filter_params, target_cols)

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
#' @param target_cols Character vector; the source columns that were filtered.
#' @return The motion_trace object with updated quality attribute.
#' @keywords internal
filt_quality_log <- function(output, method, params, target_cols = c('x', 'y', 'z')) {

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
  f_cols <- paste0('f_', target_cols)
  axes_filtered <- intersect(f_cols, names(output))
  na_counts <- vapply(axes_filtered, function(col) {
    sum(is.na(output[[col]]))
  }, integer(1))

  # Build this filter pass entry
  pass_entry <- list(
    method     = method,
    timestamp  = Sys.time(),
    parameters = params,
    target          = target_cols,
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
#' chosen signal and returns a ggplot2 scree/elbow-style plot to help
#' identify the optimal cutoff frequency.
#'
#' @param .data A motion_trace object.
#' @param method Character; filter method to evaluate (default \code{'butterworth'}).
#' @param target Character; what to analyse. One of \code{'coordinates'}
#'   (default), \code{'all_derivatives'}, \code{'velocity'},
#'   \code{'acceleration'}, \code{'angular_velocity'}, or a character vector
#'   combining any of these.
#' @param cutoff_range Numeric length-2; min and max cutoff to sweep (Hz for
#'   Butterworth; generic parameter range for other methods).
#' @param n_steps Integer; number of cutoff values to test.
#' @param axis Character; which coordinate axis to analyse when
#'   \code{target = 'coordinates'} (\code{'x'}, \code{'y'}, or \code{'z'}).
#'   Ignored for derivative targets.
#' @param plot_type Character; \code{'residual'}, \code{'psd'}, or \code{'both'}.
#' @param ... Additional arguments passed to the underlying filter
#'   (e.g. \code{n}, \code{type} for Butterworth).
#'
#' @return A \code{ggplot} object (or a patchwork composite when multiple
#'   signals are analysed).
#' @export
filtrate_cutoff <- function(.data,
                              method = 'butterworth',
                              target = 'coordinates',
                              cutoff_range = NULL,
                              n_steps = 50,
                              axis = 'x',
                              plot_type = 'both',
                              ...) {

  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package "ggplot2" is required for filtrate_cutoff(). Please install it.')
  }

  # Determine sampling rate from metadata or timestamps
  meta <- attr(.data, 'metadata')
  hz <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 10

  # Default cutoff range based on method and sampling rate
  if (is.null(cutoff_range)) {
    if (method == 'butterworth') {
      nyquist <- hz / 2
      cutoff_range <- c(0.1, nyquist * 0.95)
    } else {
      cutoff_range <- c(0.01, 0.5)
    }
  }

  # Determine which columns to analyse
  if (identical(target, 'coordinates')) {
    signal_cols <- axis
    if (!(axis %in% names(.data))) {
      stop('Axis "', axis, '" not found in data.')
    }
  } else {
    signal_cols <- .resolve_target_cols(target, .data)
  }

  # Determine x-axis label based on method
  param_label <- switch(method,
    butterworth = 'Cutoff Frequency (Hz)',
    sma         = 'Window Size',
    ema         = 'Alpha (Smoothing Factor)',
    savgol      = 'Window Size',
    'Parameter'
  )

  # Build plots for each signal column
  all_plots <- list()

  for (sig_col in signal_cols) {
    signal_vec <- as.numeric(.data[[sig_col]])
    signal_vec <- signal_vec[!is.na(signal_vec)]

    col_label <- gsub('_', ' ', sig_col)
    plots <- list()

    # Residual analysis
    if (plot_type %in% c('residual', 'both')) {
      res_df <- filt_residual_analysis(signal_vec,
                                       method = method,
                                       cutoff_range = cutoff_range,
                                       n_steps = n_steps,
                                       hz = hz,
                                       ...)

      ideal <- .est_inflection(res_df$parameter, res_df$rms_residual)

      plots$residual <- ggplot2::ggplot(res_df, ggplot2::aes(x = parameter, y = rms_residual)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 1) +
        ggplot2::geom_vline(xintercept = ideal, linetype = 'dashed', colour = 'red') +
        ggplot2::annotate('text', x = ideal, y = max(res_df$rms_residual, na.rm = TRUE),
                          label = paste0('Suggested: ', round(ideal, 4)),
                          hjust = -0.1, vjust = 1, colour = 'red', size = 3.5) +
        ggplot2::labs(
          title = paste0('Residual Analysis - ', col_label),
          x = param_label,
          y = 'RMS Residual'
        ) +
        ggplot2::theme_minimal()+
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }

    # Power spectral density
    if (plot_type %in% c('psd', 'both')) {
      psd_df <- filt_psd(signal_vec, hz = hz)

      plots$psd <- ggplot2::ggplot(psd_df,
                                   ggplot2::aes(x = frequency, y = power)) +
        ggplot2::geom_line() +
        ggplot2::scale_y_log10() +
        ggplot2::labs(
          title = paste0('Power Spectral Density - ', col_label),
          x = 'Frequency (Hz)',
          y = 'Power'
        ) +
        ggplot2::theme_minimal()+
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }

    if (plot_type == 'both') {
      all_plots <- c(all_plots, list(plots$residual), list(plots$psd))
    } else {
      all_plots <- c(all_plots, list(plots[[1]]))
    }
  }

  # Single plot -- return directly
  if (length(all_plots) == 1) return(all_plots[[1]])

  # Multiple plots -- compose with patchwork
  if (!requireNamespace('patchwork', quietly = TRUE)) {
    message('Install "patchwork" for composite plots. Returning first plot only.')
    return(all_plots[[1]])
  }

  ncol_layout <- if (plot_type == 'both') 2L else min(length(all_plots), 3L)
  Reduce('+', all_plots) + patchwork::plot_layout(ncol = ncol_layout)
}
