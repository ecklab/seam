# this function does minimal processing to statcast scraped data
# we assume that the data is **NOT** preprocessed in ANY way, like baseballr does
# statcast-utils will scrape the data in the expected format
# 1. duplicate columns are removed (why statcast provides dupes is another question...)
# 2. names are "fixed" because statcast does not supply names of both batter and pitcher
process_statcast = function(data, player_ids) {

  # find duplicated columns
  dupes = c(
    which(names(data) == "pitcher")[2],
    which(names(data) == "fielder_2")[1]
  )

  # remove duplicated columns
  data = data[, -dupes, with = FALSE]

  # add more informative name information
  data = data %>%
    dplyr::left_join(player_ids, by = c("pitcher" = "key_mlbam")) %>%
    dplyr::rename(pitcher_last = "name_last", pitcher_first = "name_first") %>%
    dplyr::left_join(player_ids, by = c("batter" = "key_mlbam")) %>%
    dplyr::rename(batter_last = "name_last", batter_first = "name_first") %>%
    dplyr::select("game_date", "pitch_type", "pitcher_last", "pitcher_first",
                  "pitcher", "batter_last", "batter_first", "batter", dplyr::everything()) %>%
    dplyr::select(-c("player_name"))

  return(data)
}

# this function takes as input data processed with process_statcast
# output is rows that are BIP, columns are columns necessary for seam
get_bip = function(statcast_pitches) {

  # attempt to get balls in play
  ## need to collapse some pitch types i think
  ## comment each line so we really know what they do
  statcast_pitches %>%
    dplyr::filter(!is.na(.data$hc_x)) %>%
    dplyr::filter(.data$hc_x != "null") %>%
    dplyr::filter(!is.na(.data$hc_y)) %>%
    dplyr::filter(.data$hc_y != "null") %>%
    dplyr::filter(!is.na(.data$launch_angle)) %>%
    dplyr::filter(.data$launch_angle != "null") %>%
    dplyr::filter(!is.na(.data$launch_speed)) %>%
    dplyr::filter(.data$launch_speed != "null") %>%
    dplyr::filter(.data$pitch_type != "KN") %>%
    dplyr::filter(.data$pitch_type != "EP") %>%
    dplyr::filter(.data$pitch_type != "SC") %>%
    dplyr::filter(.data$pitch_type != "null") %>%
    dplyr::filter(.data$game_type == "R") %>% # could be reconsidered
    dplyr::filter(.data$events != "sac_bunt") %>%
    dplyr::filter(.data$events != "sac_fly") %>%
    dplyr::filter(.data$events != "sac_fly_double_play") %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "CU" = "KC")) %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FF" = "FA")) %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FS" = "FO")) %>%
    dplyr::select("game_year", "game_date", "pitch_type", "pitcher_last",
                  "pitcher_first", "pitcher", "batter_last", "batter_first",
                  "batter", "release_speed", "release_pos_x", "release_pos_y",
                  "release_pos_z", "events", "stand", "p_throws", "pfx_x",
                  "pfx_z", "hc_x", "hc_y", "vx0", "vy0", "vz0", "ax", "ay",
                  "az", "hit_distance_sc", "launch_speed", "launch_angle",
                  "effective_speed", "release_spin_rate", "release_extension") %>%
    dplyr::mutate(release_speed = as.numeric(.data$release_speed)) %>%
    dplyr::mutate(release_pos_x = as.numeric(.data$release_pos_x)) %>%
    dplyr::mutate(release_pos_y = as.numeric(.data$release_pos_y)) %>%
    dplyr::mutate(release_pos_z = as.numeric(.data$release_pos_z)) %>%
    dplyr::mutate(pfx_x = as.numeric(.data$pfx_x)) %>%
    dplyr::mutate(pfx_z = as.numeric(.data$pfx_z)) %>%
    dplyr::mutate(hc_x = as.numeric(.data$hc_x)) %>%
    dplyr::mutate(hc_y = as.numeric(.data$hc_y)) %>%
    dplyr::mutate(vx0 = as.numeric(.data$vx0)) %>%
    dplyr::mutate(vy0 = as.numeric(.data$vy0)) %>%
    dplyr::mutate(vz0 = as.numeric(.data$vz0)) %>%
    dplyr::mutate(ax = as.numeric(.data$ax)) %>%
    dplyr::mutate(ay = as.numeric(.data$ay)) %>%
    dplyr::mutate(az = as.numeric(.data$az)) %>%
    # dplyr::filter(.data$hit_distance_sc != "null") %>%
    # dplyr::mutate(hit_distance_sc = as.numeric(.data$hit_distance_sc)) %>%
    dplyr::mutate(launch_speed = as.numeric(.data$launch_speed)) %>%
    dplyr::mutate(launch_angle = as.numeric(.data$launch_angle)) %>%
    dplyr::filter(.data$effective_speed != "null") %>%
    dplyr::mutate(effective_speed = as.numeric(.data$effective_speed)) %>%
    dplyr::filter(.data$release_spin_rate != "null") %>%
    dplyr::mutate(release_spin_rate = as.numeric(.data$release_spin_rate)) %>%
    dplyr::filter(.data$release_extension != "null") %>%
    dplyr::mutate(release_extension = as.numeric(.data$release_extension)) %>%
    dplyr::mutate(x = .data$hc_x - 125.42) %>%
    dplyr::mutate(y = 198.27 - .data$hc_y) %>%
    dplyr::select(-.data$hc_x, -.data$hc_y) %>%
    dplyr::mutate(pitch_launch_h_c = atan(.data$vx0 / .data$vy0)) %>%
    dplyr::mutate(pitch_launch_v_c = atan(.data$vx0 / sqrt(.data$vx0 ^ 2 + .data$vy0 ^ 2))) %>%
    dplyr::mutate(spray_angle = atan(.data$x / .data$y) * 180 / pi)# need to "adjust" for handedness ???

}
