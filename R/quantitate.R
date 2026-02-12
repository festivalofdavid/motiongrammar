# segment name: quantitate_logic ---

#' Quantitate Motion Grammar
#' @description A lens-based aggregator that groups by both designation and allocation.
#' @param .data Tibble output from allocate().
#' @param allocation The prefix string used in allocate() (e.g., 'match', 'p1').
#' @param designation The context level: 'section' or 'active'.
#' @param ... Expressions to pass to dplyr::summarise().
quantitate <- function(
  .data, 
  allocation = 'p1', 
  designation = 'section',
  ...
) {

  target_band_col <- paste0(allocation, 
    '_velocity_band')
  
  if (designation == 'active') {
    .data <- .data |> 
      dplyr::filter(!is.na(section_label))
  }

  res <- .data |>
    dplyr::group_by(
      section_label, 
      !!dplyr::sym(target_band_col)
    ) |>
    # 4. Open Summarise
    dplyr::summarise(
      ...,
      .groups = 'drop'
    ) |>

    dplyr::rename(intensity_band = !!dplyr::sym(target_band_col))

  return(res)
}