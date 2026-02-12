# segment name: allocate_logic ---

#' Allocate Intensity via Named Profiles
#' @description Assigns data points to bands with custom names to allow multi-set audits.
#' @param .data Tibble with velocity, acceleration, and angular_velocity.
#' @param profile A named list containing 'v_bands', 'a_bands', and 'av_bands'.
#' @param band_set_name A string to prefix the band columns (default: 'p1').
allocate <- function(
  .data, 
  profile = list(
    v_bands  = c(0, 2, 4, 6, 8),
    a_bands  = c(0, 1, 2, 3, 4),
    av_bands = c(0, 100, 200, 300, 400)
  ),
  band_set_name = 'profile_1'
) {
  
  apply_profile_bands <- function(x, thresholds) {
    if (all(is.na(x))) return(NA_character_)
    
    labels <- paste0('band_', 1:length(thresholds))
    band_idx <- findInterval(x, thresholds, all.inside = TRUE)
    return(labels[band_idx])
  }

  v_col  <- paste0(band_set_name, '_velocity_band')
  a_col  <- paste0(band_set_name, '_acceleration_band')
  av_col <- paste0(band_set_name, '_angularvelocity_band')

  res <- .data |>
    dplyr::mutate(
      !!v_col  := apply_profile_bands(velocity, profile$v_bands),
      !!a_col  := apply_profile_bands(abs(acceleration), profile$a_bands),
      !!av_col := apply_profile_bands(abs(angular_velocity), profile$av_bands)
    ) |>
    dplyr::group_by(section_label) |>
    dplyr::ungroup()
  
  return(res)
}