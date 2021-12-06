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

get_player_ids = function() {
  # really need to download a copy of this data for backup and offline use
  url = "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  data.table::fread(url, sep = ",") %>%
    dplyr::select(.data$key_mlbam, .data$name_last, .data$name_first) %>%
    dplyr::filter(!is.na(.data$key_mlbam))
}

make_p_lu = function(.bip) {
  .bip %>%
    dplyr::mutate(pitcher_name = paste(.data$pitcher_first, .data$pitcher_last)) %>%
    dplyr::select(.data$pitcher_name, .data$pitcher) %>%
    dplyr::group_by(.data$pitcher_name, .data$pitcher) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::select(.data$pitcher_name, .data$pitcher) %>%
    dplyr::distinct()
}

make_b_lu = function(.bip) {
  .bip %>%
    dplyr::mutate(batter_name = paste(.data$batter_first, .data$batter_last)) %>%
    dplyr::select(.data$batter_name, .data$batter, .data$team) %>%
    dplyr::group_by(.data$batter_name, .data$batter, .data$team) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::select(.data$batter_name, .data$batter, .data$team) %>%
    dplyr::distinct()
}
