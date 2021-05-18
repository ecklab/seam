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
make_bip_pool_pitch = function(.pitch_type, .batter, .pitcher, .bip, .batter_pool, .stand, .p_throws) {

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

  # calculate similarity for all potential donors
  # and weights?
  b_pool_sims = calc_sim_batter(b_study_char = b_study_char,
                                b_pool_char = b_pool_char,
                                ratio = 0.85)

  # append similarities to pool
  b_pool = dplyr::bind_cols(b_pool_char, b_pool_sims)

  # append
  dplyr::left_join(p_bip, b_pool, by = c("batter", "game_year")) %>%
    dplyr::select(x, y, similarity, weight)

}
