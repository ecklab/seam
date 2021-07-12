scale_this = function(x) {
  as.vector(scale(x))
}

# this should happen before the seam app
get_batter_pool = function(bip) {

  # batter pool by "year"
  batter_pool_year = bip %>%
    dplyr::group_by(batter, pitch_type, game_year, stand) %>%
    dplyr::summarise(lf_prc = mean(spray_angle < -15),
                     cf_prc = mean(spray_angle >= -15 & spray_angle <= 15),
                     rf_prc = mean(spray_angle > 15),
                     launch_angle = mean(launch_angle),
                     launch_speed = mean(launch_speed),
                     n = dplyr::n())

  # batter pool "overall"
  batter_pool_all = bip %>%
    dplyr::filter(game_year >= 2018) %>%
    dplyr::filter(game_year <= 2020) %>%
    dplyr::group_by(batter, pitch_type, stand) %>%
    dplyr::summarise(lf_prc = mean(spray_angle < -15),
                     cf_prc = mean(spray_angle >= -15 & spray_angle <= 15),
                     rf_prc = mean(spray_angle > 15),
                     launch_angle = mean(launch_angle),
                     launch_speed = mean(launch_speed),
                     n = dplyr::n())
  batter_pool_all$game_year = 0

  # merge year and overall, ungroup
  batter_pool = dplyr::ungroup(dplyr::bind_rows(batter_pool_year, batter_pool_all))

  # scale characteristic variables
  batter_pool %>%
    dplyr::mutate(
      lf_prc = scale_this(lf_prc),
      cf_prc = scale_this(cf_prc),
      rf_prc = scale_this(rf_prc),
      launch_angle = scale_this(launch_angle),
      launch_speed = scale_this(launch_speed)
    )

}

# this will be done in the seam app
make_bip_pool_synth_batter = function(.pitch_type, .batter, .pitcher, .bip, .batter_pool, .stand, .p_throws) {

  # relevant balls in play
  p_bip = .bip %>%
    dplyr::filter(batter != .batter) %>%
    dplyr::filter(pitcher == .pitcher) %>%
    dplyr::filter(p_throws == .p_throws) %>%
    dplyr::filter(stand == .stand) %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::select(game_year, batter, x, y)

  # characteristics of batter under study
  ## TODO: consider overall vs current year
  b_study_char = .batter_pool %>%
    dplyr::filter(batter == .batter) %>%
    dplyr::filter(stand == .stand) %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::filter(game_year == 0)

  # potential donor batters
  b_pool_char = .batter_pool %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::filter(stand == .stand) %>%
    dplyr::filter(game_year != 0) %>%
    dplyr::filter(batter != .batter) %>%
    dplyr::filter(batter %in% unique(p_bip$batter)) # THIS MIGHT BE "CORRECT" BUT EFFECT OTHER CALCULATIONS!!!!

  # calculate similarity and weights for all potential donors
  b_pool_sims = calc_sim_batter(b_study_char = b_study_char,
                                b_pool_char = b_pool_char,
                                ratio = 0.85)

  # append sims and weights to pool
  b_pool = dplyr::bind_cols(b_pool_char, b_pool_sims)

  # append sims and weights to bip, select relevant variables
  dplyr::left_join(p_bip, b_pool, by = c("batter", "game_year")) %>%
    dplyr::select(x, y, similarity, weight)

}

# this should happen before the seam app
get_pitcher_pool = function(bip) {

  # pitcher pool by "year"
  pitcher_pool_year = bip %>%
    dplyr::group_by(pitcher, pitch_type, game_year, p_throws) %>%
    dplyr::summarise(release_speed = mean(release_speed),
                     release_spin_rate = mean(release_spin_rate),
                     pfx_x = mean(pfx_x),
                     pfx_z = mean(pfx_z),
                     release_pos_x = mean(release_pos_x),
                     release_pos_y = mean(release_pos_y),
                     release_pos_z = mean(release_pos_z),
                     n = dplyr::n())

  # pitcher pool "overall"
  pitcher_pool_all = bip %>%
    dplyr::filter(game_year >= 2018) %>%
    dplyr::filter(game_year <= 2020) %>%
    dplyr::group_by(pitcher, pitch_type, p_throws) %>%
    dplyr::summarise(release_speed = mean(release_speed),
                     release_spin_rate = mean(release_spin_rate),
                     pfx_x = mean(pfx_x),
                     pfx_z = mean(pfx_z),
                     release_pos_x = mean(release_pos_x),
                     release_pos_y = mean(release_pos_y),
                     release_pos_z = mean(release_pos_z),
                     n = dplyr::n())
  pitcher_pool_all$game_year = 0

  # merge year and overall, ungroup
  pitcher_pool = dplyr::ungroup(dplyr::bind_rows(pitcher_pool_year, pitcher_pool_all))

  # scale characteristic variables
  pitcher_pool %>%
    dplyr::mutate(
      release_speed = scale_this(release_speed),
      release_spin_rate = scale_this(release_spin_rate),
      pfx_x = scale_this(pfx_x),
      pfx_z = scale_this(pfx_z),
      release_pos_x = scale_this(release_pos_x),
      release_pos_y = scale_this(release_pos_y),
      release_pos_z = scale_this(release_pos_z)
    )

}

# this will be done in the seam app
make_bip_pool_synth_pitcher = function(.pitch_type, .batter, .pitcher, .bip, .pitcher_pool, .stand, .p_throws) {

  # relevant balls in play
  b_bip = .bip %>%
    dplyr::filter(batter == .batter) %>%
    dplyr::filter(pitcher != .pitcher) %>%
    dplyr::filter(p_throws == .p_throws) %>%
    dplyr::filter(stand == .stand) %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::select(game_year, pitcher, x, y)

  # characteristics of batter under study
  ## TODO: consider overall vs current year
  p_study_char = .pitcher_pool %>%
    dplyr::filter(pitcher == .pitcher) %>%
    dplyr::filter(p_throws == .p_throws) %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::filter(game_year == 0)

  # potential donor pitchers
  p_pool_char = .pitcher_pool %>%
    dplyr::filter(pitch_type == .pitch_type) %>%
    dplyr::filter(p_throws == .p_throws) %>%
    dplyr::filter(game_year != 0) %>%
    dplyr::filter(pitcher != .pitcher) %>%
    dplyr::filter(pitcher %in% unique(b_bip$pitcher)) # THIS MIGHT BE "CORRECT" BUT EFFECT OTHER CALCULATIONS!!!!

  # # temp debugging
  # list(p_bip, p_pool_char)

  # calculate similarity and weights for all potential donors
  p_pool_sims = calc_sim_pitcher(p_study_char = p_study_char,
                                p_pool_char = p_pool_char,
                                ratio = 0.85)

  # append sims and weights to pool
  p_pool = dplyr::bind_cols(p_pool_char, p_pool_sims)

  # append sims and weights to bip, select relevant variables
  dplyr::left_join(b_bip, p_pool, by = c("pitcher", "game_year")) %>%
    dplyr::select(x, y, similarity, weight)

}
