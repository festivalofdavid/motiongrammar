# segment name: filtrate ---

#' Filter Motion Trace Noise on xyz coordinates
#' 
#' @description
#' Applies digital filters to x, y, and z coordinates to remove GPS jitter.
#' 
#' @param .data A motion_trace object.
#' @param method Character; the filtering algorithm. Just using butterworth right now
#' @param n Filter order
#' @param cutoff Filter frequency
#' @param type filter type (ie., high or low pass)
#' 
#' @return A filtered \code{motion_trace} object.
#' @export
filtrate <- function(.data, 
                     method = 'butterworth', 
                     n = 2, 
                     cutoff = 0.1, 
                     type = 'low'){
  
  # Validation: Ensure coordinates exist
  if (!all(c('x', 'y') %in% names(.data))) {
    stop('Coordinates missing. Run coordinate() before filtrate().')
  }

  #  Filter Selection Logic ---
  if (method == 'butterworth') {
    
    # Make sure we have signal package installed
    if (!requireNamespace('signal', quietly = TRUE)){
      stop('Package "signal" is required for Butterworth filtering. Please install it.')
    }
    
    # Run butterworth according to our specifications
    bf <- signal::butter(n = n, W = cutoff, type = type)
    
    # 2. Apply the filter using filtfilt
    output <- .data |>
      dplyr::mutate(
        f_x = as.numeric(signal::filtfilt(bf, x)),
        f_y = as.numeric(signal::filtfilt(bf, y)),
        f_z = if ('z' %in% names(.data)) as.numeric(signal::filtfilt(bf, z)) else z
      )
  } 
  
  else {
    stop('Method "', method, '" not yet implemented.')
  }

  # Preserve class and update metadata
  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }
  
  attr(output, 'metadata')$filter_applied <- method
  
  return(output)
}