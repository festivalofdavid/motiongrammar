# segment name: designate_logic ---
#' designate Motion Data
#' @description Finalises segmentation using statistical masking and type-safe combining.
#' @param .data Tibble with 'velocity', 'unix_time', and 'is_interpolated'.
#' @param v_threshold Velocity (m/s) to distinguish active vs downtime.
#' @param min_pts Minimum segment length (e.g., 300 pts = 30s at 10Hz).
designate <- function(.data, 
  v_threshold = 1, 
  min_pts = 300,
  method = 'corbett'
) {
  
  if(method == 'corbett'){
  # 1. Filter so only finite values-- PELT cannot handle NA's.
  .data <- .data |>
    dplyr::filter(
      is.finite(velocity)
    )
  
  if (nrow(.data) == 0) {
    stop('designate(): no finite velocity values left after filtering.')
  }
  
  # 2. Get PELT statistical bounds
  m_pelt <- changepoint::cpt.meanvar(
    data      = .data$velocity, 
    method    = 'PELT', 
    penalty   = 'Manual',
    pen.value = 5
  )
  
  # 3. Categorise by chagepoint
  processed <- .data |>
    dplyr::mutate(
      pelt_id = findInterval(
        dplyr::row_number(), 
        c(0, changepoint::cpts(m_pelt))
      )
    ) |>
    dplyr::group_by(pelt_id) |>
    dplyr::mutate(
      is_downtime = mean(velocity, na.rm = TRUE) < v_threshold
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      type_group_id = dplyr::consecutive_id(is_downtime)
    )
  
  # 4. Combine tiny segments together
  refined <- processed |>
    dplyr::group_by(type_group_id) |>
    dplyr::mutate(
      segment_size = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      refined_id = ifelse(
        segment_size < min_pts, 
        NA_real_, 
        as.numeric(type_group_id)
      ),
      refined_id = zoo::na.locf(refined_id, na.rm = FALSE),
      refined_id = tidyr::replace_na(refined_id, 1)
    ) |>
    # 5. Final Semantic Labelling
    dplyr::group_by(refined_id) |>
    dplyr::mutate(
      final_mean = mean(velocity, na.rm = TRUE),
      is_static_final = final_mean < v_threshold
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_prefix = ifelse(is_static_final, 'downtime_', 'active_'),
      section_label = paste0(
        label_prefix, 
        dplyr::consecutive_id(refined_id)
      )
    ) |> 
    select(-pelt_id:-label_prefix)
  }
  
  return(refined)
}