# function to obtain first and last name for players with an MLBAM identifier
# https://github.com/chadwickbureau
get_player_ids = function() {
  url = "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
  data.table::fread(url, sep = ",") %>%
    dplyr::select(key_mlbam, name_last, name_first) %>%
    dplyr::filter(!is.na(key_mlbam))
}
