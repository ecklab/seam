#' Download and processes MLB player IDs
#'
#' @return A data table with MLBAM, First Name, and Last Name
#' @export
#'
#' @importFrom rlang .data
get_player_ids = function() {
  url = "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  data.table::fread(url, sep = ",") %>%
    dplyr::select(.data$key_mlbam, .data$name_last, .data$name_first) %>%
    dplyr::filter(!is.na(.data$key_mlbam))
}

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

make_p_lu = function(.bip) {
  .bip %>%
    dplyr::mutate(pitcher_name = paste(.data$pitcher_first, .data$pitcher_last)) %>%
    dplyr::select(.data$pitcher_name, .data$pitcher) %>%
    dplyr::distinct()
}

make_b_lu = function(.bip) {
  .bip %>%
    dplyr::mutate(batter_name = paste(.data$batter_first, .data$batter_last)) %>%
    dplyr::select(.data$batter_name, .data$batter) %>%
    dplyr::distinct()
}