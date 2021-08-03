#' @importFrom rlang .data
lu_p = function(.p_lu, .pitcher_name) {

  p_info = .p_lu %>%
    dplyr::filter(.data$pitcher_name == .pitcher_name)

  if (length(p_info$pitcher) != 1) {
    warning("Non-unique name specified.")
  }

  return(p_info$pitcher)
}

lu_b = function(.b_lu, .batter_name) {

  b_info = .b_lu %>%
    dplyr::filter(.data$batter_name == .batter_name)

  if (length(b_info$batter) != 1) {
    warning("Non-unique name specified.")
  }

  return(b_info$batter)
}
