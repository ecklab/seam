pitches_processed %>%
  dplyr::filter(lubridate::year(game_date) >= 2017) %>%
  dplyr::filter(lubridate::year(game_date) <= 2019) %>%
  dplyr::filter(game_type == "R") %>%
  dplyr::filter(!is.na(events)) %>%
  dplyr::filter(events != "") #!!!!! THIS IS IMPORTANT, COULD BE EFFECTING DATA IN USE

################################################################################

pitches_processed_sub = pitches_processed %>%
  dplyr::select("events", "pitcher", "pitch_type")


pitches_processed_sub %>%
  dplyr::filter(.data$pitch_type != "") %>%
  dplyr::filter(.data$pitch_type != "KN") %>%
  dplyr::filter(.data$pitch_type != "EP") %>%
  dplyr::filter(.data$pitch_type != "SC") %>%
  dplyr::filter(.data$pitch_type != "IN") %>%
  dplyr::filter(.data$pitch_type != "PO") %>%
  dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "CU" = "KC")) %>%
  dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FF" = "FA")) %>%
  dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FS" = "FO")) %>%
  dplyr::filter(pitcher == lu_p(p_lu, "Justin Verlander")) %>%
  dplyr::group_by(.data$pitch_type) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(freq_pitches = .data$n / sum(.data$n)) %>%  # this needs to be based on pitches instead
  dplyr::select(pitch_type, freq_pitches)

################################################################################

a = bip[pitcher == 434378][, .(n = .N), by = pitch_type]
a[, freq := n / sum(n)][]

calc_props = function(x) {
  x / sum(x)
}

################################################################################

get_na_perc = function(x) {
  mean(is.na(x))
}

na_perc = sapply(pitches_processed, get_na_perc)
names(pitches_processed)[na_perc == 1]

################################################################################

get_null_perc = function(x) {
  mean(x == "null")
}

sapply(pitches_processed[, -1], get_null_perc)

################################################################################

all_vars = c(
  "game_date",
  "pitch_type",
  "pitcher_last",
  "pitcher_first",
  "pitcher",
  "batter_last",
  "batter_first",
  "batter",
  "release_speed",
  "release_pos_x",
  "release_pos_z",
  "events",
  "description",
  "spin_dir",
  "spin_rate_deprecated",
  "break_angle_deprecated",
  "break_length_deprecated",
  "zone",
  "des",
  "game_type",
  "stand",
  "p_throws",
  "home_team",
  "away_team",
  "type",
  "hit_location",
  "bb_type",
  "balls",
  "strikes",
  "game_year",
  "pfx_x",
  "pfx_z",
  "plate_x",
  "plate_z",
  "on_3b",
  "on_2b",
  "on_1b",
  "outs_when_up",
  "inning",
  "inning_topbot",
  "hc_x",
  "hc_y",
  "tfs_deprecated",
  "tfs_zulu_deprecated",
  "umpire",
  "sv_id",
  "vx0",
  "vy0",
  "vz0",
  "ax",
  "ay",
  "az",
  "sz_top",
  "sz_bot",
  "hit_distance_sc",
  "launch_speed",
  "launch_angle",
  "effective_speed",
  "release_spin_rate",
  "release_extension",
  "game_pk",
  "fielder_2",
  "fielder_3",
  "fielder_4",
  "fielder_5",
  "fielder_6",
  "fielder_7",
  "fielder_8",
  "fielder_9",
  "release_pos_y",
  "estimated_ba_using_speedangle",
  "estimated_woba_using_speedangle",
  "woba_value",
  "woba_denom",
  "babip_value",
  "iso_value",
  "launch_speed_angle",
  "at_bat_number",
  "pitch_number",
  "pitch_name",
  "home_score",
  "away_score",
  "bat_score",
  "fld_score",
  "post_away_score",
  "post_home_score",
  "post_bat_score",
  "post_fld_score",
  "if_fielding_alignment",
  "of_fielding_alignment"
)

vars = c(
  "game_date",
  "pitch_type",
  "pitcher_last",
  "pitcher_first",
  "pitcher",
  "batter_last",
  "batter_first",
  "batter",
  "release_speed",
  "release_pos_x",
  "release_pos_z",
  "events",
  "description",
  "spin_dir",
  "spin_rate_deprecated",
  "break_angle_deprecated",
  "break_length_deprecated",
  "zone",
  "des",
  "game_type",
  "stand",
  "p_throws",
  "home_team",
  "away_team",
  "type",
  "hit_location",
  "bb_type",
  "balls",
  "strikes",
  "game_year",
  "pfx_x",
  "pfx_z",
  "plate_x",
  "plate_z",
  "on_3b",
  "on_2b",
  "on_1b",
  "outs_when_up",
  "inning",
  "inning_topbot",
  "hc_x",
  "hc_y",
  "tfs_deprecated",
  "tfs_zulu_deprecated",
  "umpire",
  "sv_id",
  "vx0",
  "vy0",
  "vz0",
  "ax",
  "ay",
  "az",
  "sz_top",
  "sz_bot",
  "hit_distance_sc",
  "launch_speed",
  "launch_angle",
  "effective_speed",
  "release_spin_rate",
  "release_extension",
  "game_pk",
  "fielder_2",
  "fielder_3",
  "fielder_4",
  "fielder_5",
  "fielder_6",
  "fielder_7",
  "fielder_8",
  "fielder_9",
  "release_pos_y",
  "estimated_ba_using_speedangle",
  "estimated_woba_using_speedangle",
  "woba_value",
  "woba_denom",
  "babip_value",
  "iso_value",
  "launch_speed_angle",
  "at_bat_number",
  "pitch_number",
  "pitch_name",
  "home_score",
  "away_score",
  "bat_score",
  "fld_score",
  "post_away_score",
  "post_home_score",
  "post_bat_score",
  "post_fld_score",
  "if_fielding_alignment",
  "of_fielding_alignment"
)
