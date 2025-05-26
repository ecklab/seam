# this function does minimal processing to statcast scraped data
# we assume that the data is **NOT** preprocessed in ANY way, like baseballr does
# statcast-utils will scrape the data in the expected format
# 1. duplicate columns are removed (why statcast provides dupes is another question...)
# 2. names are "fixed" because statcast does not supply names of both batter and pitcher
# TODO: this description needs updating. we are now doing some semi-opinionated processing
process_statcast = function(data, player_ids) {

  # find duplicated columns
  dupes = c(
    which(names(data) == "pitcher")[2],    # NOTE: this is bc there's no "pitcher column"
    which(names(data) == "fielder_2")[1]
  )

  # remove duplicated columns FIXME:
  # data = data[, !dupes, with = FALSE]

  # remove bad statcast years (2015-2016)
  # currently exclude 2021 until maybe the all star break
  # or completely hold back for validation
  data = data %>%
    dplyr::filter(lubridate::year(game_date) >= 2021) %>%
    dplyr::filter(lubridate::year(game_date) <= 2024)

  # View(head(data))
  for (name in names(data)) {
    if (startsWith(name, "pitch")) {
      print(name)
    }    # FIXME: need to comment out and delete this later
  }
  # View(head(player_ids))

  # add more informative name information
  data = data %>%
    dplyr::rename(batter = "batter_id", pitcher = "pitcher_id", p_throws = "pitch_hand") %>%
    dplyr::left_join(player_ids, by = c("pitcher" = "key_mlbam")) %>%    # FIXME: "pitcher" column not present in dataset
    dplyr::rename(pitcher_last = "name_last", pitcher_first = "name_first") %>%
    dplyr::left_join(player_ids, by = c("batter" = "key_mlbam")) %>%
    dplyr::rename(game_year = "year") %>%
    dplyr::rename(stand = "bat_side") %>%
    dplyr::rename(release_extension = "extension") %>%
    dplyr::rename(batter_last = "name_last", batter_first = "name_first") %>%
    dplyr::select("game_date", "pitch_type", "pitcher_last", "pitcher_first",
                  "pitcher", "batter_last", "batter_first", "batter", dplyr::everything())
    # dplyr::select(-c("player_name"))

  # View(head(data))

  # temporarily extract/remove game data
  game_date = data$game_date
  data = data[, -1]

  # replace "null" with NA
  data[data == "null"] = NA

  # recombine data
  data = dplyr::bind_cols(data.frame(game_date = game_date), data)

  # variables that are 100% NA
  na_vars = c(
    "spin_dir",
    "spin_rate_deprecated",
    "break_angle_deprecated",
    "break_length_deprecated",
    "tfs_deprecated",
    "tfs_zulu_deprecated",
    "umpire"
  )

  # View(head(data)) TODO: uncomment when not debugging

  # remove variables that do not return values
  data = data %>%
    dplyr::select(-dplyr::all_of(na_vars))

  # coerce numeric data to be numeric
  data$release_speed                   = as.numeric(data$release_speed)
  data$release_pos_x                   = as.numeric(data$release_pos_x)
  data$release_pos_z                   = as.numeric(data$release_pos_z)
  data$pfx_x                           = as.numeric(data$pfx_x)
  data$pfx_z                           = as.numeric(data$pfx_z)
  data$plate_x                         = as.numeric(data$plate_x)
  data$plate_z                         = as.numeric(data$plate_z)
  data$hc_x                            = as.numeric(data$hit_coord_x)
  data$hc_y                            = as.numeric(data$hit_coord_y)
  data$vx0                             = as.numeric(data$vx0)
  data$vy0                             = as.numeric(data$vy0)
  data$vz0                             = as.numeric(data$vz0)
  data$ax                              = as.numeric(data$ax)
  data$ay                              = as.numeric(data$ay)
  data$az                              = as.numeric(data$az)
  # data$sz_top                          = as.numeric(data$sz_top)
  # data$sz_bot                          = as.numeric(data$sz_bot)
  data$hit_distance_sc                 = as.numeric(data$hit_distance_sc)
  data$launch_speed                    = as.numeric(data$launch_speed)
  data$launch_angle                    = as.numeric(data$launch_angle)
  data$effective_speed                 = as.numeric(data$effective_speed)
  data$release_spin_rate               = as.numeric(data$release_spin_rate)
  data$release_extension               = as.numeric(data$release_extension)
  data$release_pos_y                   = as.numeric(data$release_pos_y)
  # data$estimated_ba_using_speedangle   = as.numeric(data$estimated_ba_using_speedangle)
  # data$estimated_woba_using_speedangle = as.numeric(data$estimated_woba_using_speedangle)
  data$woba_value                      = as.numeric(data$woba_value)
  data$woba_denom                      = as.numeric(data$woba_denom)
  data$babip_value                     = as.numeric(data$babip_value)
  data$iso_value                       = as.numeric(data$iso_value)
  data$launch_speed_angle              = as.numeric(data$launch_speed_angle)

  to_get_teams = data %>%
    dplyr::select(game_date, batter, home_team, away_team, half_inning) %>%
    dplyr::arrange(dplyr::desc(game_date)) %>%
    dplyr::mutate(batter_team = ifelse(half_inning == "top", away_team, home_team)) %>%
    dplyr::group_by(batter) %>%
    dplyr::summarise(team = names(which.max(table(batter_team))))

  data = data %>%
    dplyr::left_join(to_get_teams, by = "batter")

  # return result
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
    dplyr::filter(!is.na(.data$hc_y)) %>%
    dplyr::filter(!is.na(.data$launch_angle)) %>%
    dplyr::filter(!is.na(.data$launch_speed)) %>%
    dplyr::filter(!is.na(.data$pitch_type)) %>%
    dplyr::filter(.data$pitch_type != "") %>%
    dplyr::filter(.data$pitch_type != "KN") %>%
    dplyr::filter(.data$pitch_type != "EP") %>%
    dplyr::filter(.data$pitch_type != "SC") %>%
    dplyr::filter(.data$game_type == "R") %>% # could be reconsidered
    dplyr::filter(.data$events != "sac_bunt") %>% # could be reconsidered
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
                  "az", "launch_speed", "launch_angle", "effective_speed",
                  "release_spin_rate", "release_extension", "team") %>%
    dplyr::filter(!is.na(.data$effective_speed)) %>%
    dplyr::mutate(effective_speed = as.numeric(.data$effective_speed)) %>%
    dplyr::filter(!is.na(.data$release_spin_rate)) %>%
    dplyr::mutate(release_spin_rate = as.numeric(.data$release_spin_rate)) %>%
    dplyr::filter(!is.na(.data$release_extension)) %>%
    dplyr::mutate(release_extension = as.numeric(.data$release_extension)) %>%
    dplyr::mutate(x = .data$hc_x - 125.42) %>%
    dplyr::mutate(y = 198.27 - .data$hc_y) %>%
    dplyr::select(-.data$hc_x, -.data$hc_y) %>%
    dplyr::mutate(pitch_launch_h_c = atan(.data$vx0 / .data$vy0)) %>%
    dplyr::mutate(pitch_launch_v_c = atan(.data$vx0 / sqrt(.data$vx0 ^ 2 + .data$vy0 ^ 2))) %>%
    dplyr::mutate(spray_angle = atan(.data$x / .data$y) * 180 / pi) # need to "adjust" for handedness ???

}

# NOTE: new api variable names...
#       hc_x is hit_coord_x
#       hc_y is hit_coord_y
#       vx0, vy0, vz0 remain the same
#       ax, ay remain the same
#       p_throws is pitch_hand
#       launch_speed remains the same
#       

# NOTE: baseball savant variable test...
#       game_year mutated to year
#       game_date remains the same
#       pitch_type remains the same
#       pitcher_id mutated to get pitcher_last & pitcher_first
#       batter_name mutated to get batter_last & batter_first
#       batter_id remains the same
#       release_speed remains the same
#       release_pos_x remains the same
#       release_pos_y remains the same
#       release_pos_z remains the same
#       events remains the same
#       stand mutates to bat_side 
#       p_throws mutates to pitch_hand
#       pfx_x remains the same
#       pfx_z remains the same
#       hc_x mutates to hit_coord_x
#       hc_y muattes to hit_coord_y
#       vx0 remains the same
#       vy0 remains the same
#       vz0 remains the same
#       ax remains the same
#       ay remains the same
#       az remains the same
#       launch_speed remains the same
#       launch_angle remains the same
#       effective_speed remains the same
#       release_spin_rate remains the same
#       release_extension mutates to extension
#       team mutates in process_statcast()
# NOTE: inning_topbot is now half_inning
