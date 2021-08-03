calc_n_synth = function(df) {
  sum(df$similarity ^ 2)
}

do_full_seam_matchup = function(.batter, .pitcher, .pitches, .bip, .batter_pool, .pitcher_pool) {

  pitcher_pitches = get_pitcher_pitches(.bip = .bip,
                                        .pitches = .pitches,
                                        .pitcher = .pitcher)

  hands = get_matchup_hands(
    bip = bip,
    b_id = .batter,
    p_id = .pitcher
  )

  # true matchup ###############################################################

  empirical_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = bip
  )

  n = nrow(empirical_pool)

  empirical_df = empirical_pool %>%
    kde_helper() %>%
    kde_to_df()

  # synth batter ###############################################################

  synth_batter_pools = lapply(
    pitcher_pitches$pitch_type,
    make_bip_pool_synth_batter,
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = bip,
    .batter_pool = .batter_pool,
    .stand = hands["b_stands"],
    .p_throws = hands["p_throws"]
  )

  n_b = sum(sapply(synth_batter_pools, calc_n_synth))

  synth_batter_z = lapply(synth_batter_pools, kde_helper) %>%
    lapply(kde_to_df) %>%
    lapply(kde_z_extractor) %>%
    unlist() %>%
    matrix(ncol = length(synth_batter_pools)) %*%
    pitcher_pitches$freq_pitches %>%
    as.vector()

  # synth pitcher ##############################################################

  synth_pitcher_pools = lapply(
    pitcher_pitches$pitch_type,
    make_bip_pool_synth_pitcher,
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = bip,
    .pitcher_pool = .pitcher_pool,
    .stand = hands["b_stands"],
    .p_throws = hands["p_throws"]
  )

  n_p = sum(sapply(synth_pitcher_pools, calc_n_synth))

  synth_pitcher_z = lapply(synth_pitcher_pools, kde_helper) %>%
    lapply(kde_to_df) %>%
    lapply(kde_z_extractor) %>%
    unlist() %>%
    matrix(ncol = length(synth_pitcher_pools)) %*%
    pitcher_pitches$freq_pitches %>%
    as.vector()

  # combine ####################################################################

  lambda = sqrt(n) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_p = sqrt(n_p) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_b = sqrt(n_b) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))

  seam_df = empirical_df

  seam_df$z =
    lambda * empirical_df$z +
    lambda_b * synth_batter_z +
    lambda_p * synth_pitcher_z

  return(seam_df)

}
