scale_this = function(x) {
  as.vector(scale(x))
}

# this should happen before the seam app
get_batter_pool = function(bip, year_start = 2017, year_end = 2021) {

  bip = bip %>%
    dplyr::filter(.data$game_year >= year_start) %>%
    dplyr::filter(.data$game_year <= year_end)

  # batter pool by "year"
  batter_pool_year = bip %>%
    dplyr::group_by(.data$batter, .data$pitch_type, .data$game_year, .data$stand) %>%
    dplyr::summarise(lf_prc = mean(.data$spray_angle < -15),
                     cf_prc = mean(.data$spray_angle >= -15 & .data$spray_angle <= 15),
                     rf_prc = mean(.data$spray_angle > 15),
                     launch_angle = mean(.data$launch_angle),
                     launch_speed = mean(.data$launch_speed),
                     n = dplyr::n())

  # batter pool "overall"
  batter_pool_all = bip %>%
    dplyr::group_by(.data$batter, .data$pitch_type, .data$stand) %>%
    dplyr::summarise(lf_prc = mean(.data$spray_angle < -15),
                     cf_prc = mean(.data$spray_angle >= -15 & .data$spray_angle <= 15),
                     rf_prc = mean(.data$spray_angle > 15),
                     launch_angle = mean(.data$launch_angle),
                     launch_speed = mean(.data$launch_speed),
                     n = dplyr::n())
  batter_pool_all$game_year = 0

  # merge year and overall, ungroup
  batter_pool = dplyr::ungroup(dplyr::bind_rows(batter_pool_year, batter_pool_all))

  # scale characteristic variables
  batter_pool %>%
    dplyr::mutate(
      lf_prc = scale_this(.data$lf_prc),
      cf_prc = scale_this(.data$cf_prc),
      rf_prc = scale_this(.data$rf_prc),
      launch_angle = scale_this(.data$launch_angle),
      launch_speed = scale_this(.data$launch_speed)
    )

}

# this will be done in the seam app
make_bip_pool_synth_batter = function(.pitch_type, .batter, .pitcher, .bip, .batter_pool, .stand, .p_throws, .ratio) {

  # relevant balls in play
  p_bip = .bip %>%
    dplyr::filter(.data$batter != .batter) %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::filter(.data$p_throws == .p_throws) %>%
    dplyr::filter(.data$stand == .stand) %>%
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::select(.data$game_year, .data$batter, .data$x, .data$y)

  # characteristics of batter under study
  ## TODO: consider overall vs current year
  b_study_char = .batter_pool %>%
    dplyr::filter(.data$batter == .batter) %>%
    dplyr::filter(.data$stand == .stand) %>%
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::filter(.data$game_year == 0)

  # potential donor batters
  b_pool_char = .batter_pool %>%
    dplyr::filter(.data$batter %in% unique(p_bip$batter)) %>% # THIS MIGHT BE "CORRECT" BUT EFFECT OTHER CALCULATIONS!!!!
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::filter(.data$stand == .stand) %>%
    dplyr::filter(.data$game_year != 0)

  # calculate similarity and weights for all potential donors
  b_pool_sims = calc_sim_batter(b_study_char = b_study_char,
                                b_pool_char = b_pool_char,
                                ratio = .ratio)

  # append sims and weights to pool
  b_pool = dplyr::bind_cols(b_pool_char, b_pool_sims)

  # append sims and weights to bip, select relevant variables
  dplyr::left_join(p_bip, b_pool, by = c("batter", "game_year")) %>%
    dplyr::select(.data$x, .data$y, .data$similarity, .data$weight)

}

# this should happen before the seam app
get_pitcher_pool = function(bip, year_start = 2017, year_end = 2021) {

  bip = bip %>%
    dplyr::filter(.data$game_year >= year_start) %>%
    dplyr::filter(.data$game_year <= year_end)

  # pitcher pool by "year"
  pitcher_pool_year = bip %>%
    dplyr::group_by(.data$pitcher, .data$pitch_type, .data$game_year, .data$p_throws) %>%
    dplyr::summarise(release_speed = mean(.data$release_speed),
                     release_spin_rate = mean(.data$release_spin_rate),
                     pfx_x = mean(.data$pfx_x),
                     pfx_z = mean(.data$pfx_z),
                     release_pos_x = mean(.data$release_pos_x),
                     release_pos_y = mean(.data$release_pos_y),
                     release_pos_z = mean(.data$release_pos_z),
                     n = dplyr::n())

  # pitcher pool "overall"
  pitcher_pool_all = bip %>%
    dplyr::group_by(.data$pitcher, .data$pitch_type, .data$p_throws) %>%
    dplyr::summarise(release_speed = mean(.data$release_speed),
                     release_spin_rate = mean(.data$release_spin_rate),
                     pfx_x = mean(.data$pfx_x),
                     pfx_z = mean(.data$pfx_z),
                     release_pos_x = mean(.data$release_pos_x),
                     release_pos_y = mean(.data$release_pos_y),
                     release_pos_z = mean(.data$release_pos_z),
                     n = dplyr::n())
  pitcher_pool_all$game_year = 0

  # merge year and overall, ungroup
  pitcher_pool = dplyr::ungroup(dplyr::bind_rows(pitcher_pool_year, pitcher_pool_all))

  # scale characteristic variables
  pitcher_pool %>%
    dplyr::mutate(
      release_speed = scale_this(.data$release_speed),
      release_spin_rate = scale_this(.data$release_spin_rate),
      pfx_x = scale_this(.data$pfx_x),
      pfx_z = scale_this(.data$pfx_z),
      release_pos_x = scale_this(.data$release_pos_x),
      release_pos_y = scale_this(.data$release_pos_y),
      release_pos_z = scale_this(.data$release_pos_z)
    )

}

# this will be done in the seam app
make_bip_pool_synth_pitcher = function(.pitch_type, .batter, .pitcher, .bip, .pitcher_pool, .stand, .p_throws, .ratio) {

  # relevant balls in play
  b_bip = .bip %>%
    dplyr::filter(.data$batter == .batter) %>%
    dplyr::filter(.data$pitcher != .pitcher) %>%
    dplyr::filter(.data$p_throws == .p_throws) %>%
    dplyr::filter(.data$stand == .stand) %>%
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::select(.data$game_year, .data$pitcher, .data$x, .data$y)

  # characteristics of batter under study
  ## TODO: consider overall vs current year
  p_study_char = .pitcher_pool %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::filter(.data$p_throws == .p_throws) %>%
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::filter(.data$game_year == 0)

  # potential donor pitchers
  p_pool_char = .pitcher_pool %>%
    dplyr::filter(.data$pitcher %in% unique(b_bip$pitcher)) %>%  # THIS MIGHT BE "CORRECT" BUT EFFECT OTHER CALCULATIONS!!!!
    dplyr::filter(.data$pitch_type == .pitch_type) %>%
    dplyr::filter(.data$p_throws == .p_throws) %>%
    dplyr::filter(.data$game_year != 0)

  # calculate similarity and weights for all potential donors
  p_pool_sims = calc_sim_pitcher(p_study_char = p_study_char,
                                 p_pool_char = p_pool_char,
                                 ratio = .ratio)

  # append sims and weights to pool
  p_pool = dplyr::bind_cols(p_pool_char, p_pool_sims)

  # append sims and weights to bip, select relevant variables
  dplyr::left_join(b_bip, p_pool, by = c("pitcher", "game_year")) %>%
    dplyr::select(.data$x, .data$y, .data$similarity, .data$weight)

}

make_empirical_pool = function(.batter, .pitcher, .bip, type, hands) {

  if (type == "batter") {
    pool = .bip %>%
      dplyr::filter(.data$batter == .batter) %>%
      dplyr::filter(.data$p_throws == hands[["p_throws"]])
  }

  if (type == "pitcher") {
    pool = .bip %>%
      dplyr::filter(.data$pitcher == .pitcher) %>%
      dplyr::filter(.data$stand == hands[["b_stands"]])
  }

  if (type == "both") {
    pool = .bip %>%
      dplyr::filter(.data$batter == .batter) %>%
      dplyr::filter(.data$pitcher == .pitcher)
  }

  # consider only returning relevant variables, notably x and y
  return(pool)

}
