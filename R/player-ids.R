#' Computes a tidy correlation
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
