calc_n_synth = function(df, d = 2) {
  sum(df$similarity ^ d)
}

#' @export
do_full_seam_matchup = function(.batter, .pitcher, .bip, .batter_pool, .pitcher_pool, .ratio_batter, .ratio_pitcher, .d = 2) {

  hands = get_matchup_hands(
    bip = .bip,
    b_id = .batter,
    p_id = .pitcher
  )

  pitcher_pitches = get_pitcher_pitches(.bip = .bip,
                                        .pitcher = .pitcher,
                                        .stands = hands[["b_stands"]])

  batter_pitches = get_batter_pitches(.bip = .bip,
                                      .batter = .batter,
                                      .p_throws = hands[["p_throws"]])

  # print(pitcher_pitches)
  # print(batter_pitches)

  # TODO: find where in the app this happens???
  # TODO: confirmed this only happens in the app, not internal functions
  # print("THIS SHOULD ONLY PRINT ONCE")

  if (sum(batter_pitches$n) <= 1) {
    status = "Not enough batter pitches."
  }

  pt_int = all(pitcher_pitches$pitch_type %in% batter_pitches$pitch_type)

  # vs all #####################################################################

  empirical_pitcher_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "pitcher",
    hands = hands
  )

  empirical_pitcher_df = empirical_pitcher_pool %>%
    kde_helper() %>%
    kde_to_df()

  empirical_batter_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "batter",
    hands = hands
  )

  empirical_batter_df = empirical_batter_pool %>%
    kde_helper() %>%
    kde_to_df()

  empirical_both_df = empirical_batter_df
  empirical_both_df$z = 0.5 * empirical_batter_df$z + 0.5 * empirical_pitcher_df$z

  # true matchup ###############################################################

  empirical_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "both",
    hands = hands
  )

  n = nrow(empirical_pool)

  if (n == 0 | n == 1) {
    empirical_df = empirical_batter_df
    empirical_df$z = 0
  }

  if (n > 1) {
    empirical_df = empirical_pool %>%
      kde_helper() %>%
      kde_to_df()
  }

  # synth batter ###############################################################

  synth_batter_df = empirical_pitcher_df
  n_b = nrow(empirical_pitcher_pool)

  if (pt_int) {

    synth_batter_pools = lapply(
      pitcher_pitches$pitch_type,
      make_bip_pool_synth_batter,
      .batter = .batter,
      .pitcher = .pitcher,
      .bip = .bip,
      .batter_pool = .batter_pool,
      .stand = hands["b_stands"],
      .p_throws = hands["p_throws"],
      .ratio = .ratio_batter
    )

    n_b = sum(sapply(synth_batter_pools, calc_n_synth, d = .d))

    # synth_batter_z = lapply(synth_batter_pools, kde_helper) %>%
    #   lapply(kde_to_df) %>%
    #   lapply(kde_z_extractor) %>%
    #   unlist() %>%
    #   matrix(ncol = length(synth_batter_pools)) %*%
    #   pitcher_pitches$freq_pitches %>%
    #   as.vector()

    synth_batter_pools = purrr::map2(synth_batter_pools,
                                      pitcher_pitches$freq_pitches,
                                      re_weight)

    synth_batter_pools = dplyr::bind_rows(synth_batter_pools)

    synth_batter_z = kde_helper(synth_batter_pools) %>%
      kde_to_df %>%
      kde_z_extractor


    synth_batter_df$z = synth_batter_z

  }

  # synth pitcher ##############################################################

  synth_pitcher_df = empirical_batter_df
  n_p = nrow(empirical_batter_pool)

  if (pt_int) {

    synth_pitcher_pools = lapply(
      pitcher_pitches$pitch_type,
      make_bip_pool_synth_pitcher,
      .batter = .batter,
      .pitcher = .pitcher,
      .bip = .bip,
      .pitcher_pool = .pitcher_pool,
      .stand = hands["b_stands"],
      .p_throws = hands["p_throws"],
      .ratio = .ratio_pitcher
    )

    n_p = sum(sapply(synth_pitcher_pools, calc_n_synth, d = .d))

    # synth_pitcher_z = lapply(synth_pitcher_pools, kde_helper) %>%
    #   lapply(kde_to_df) %>%
    #   lapply(kde_z_extractor) %>%
    #   unlist() %>%
    #   matrix(ncol = length(synth_pitcher_pools)) %*%
    #   pitcher_pitches$freq_pitches %>%
    #   as.vector()

    synth_pitcher_pools = purrr::map2(synth_pitcher_pools,
                                      pitcher_pitches$freq_pitches,
                                      re_weight)

    synth_pitcher_pools = dplyr::bind_rows(synth_pitcher_pools)

    synth_pitcher_z = kde_helper(synth_pitcher_pools) %>%
        kde_to_df %>%
        kde_z_extractor

    synth_pitcher_df$z = synth_pitcher_z

  }

  # combine ####################################################################

  # print(c(n = n, n_b = n_b, n_p = n_p))

  lambda = sqrt(n) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_p = sqrt(n_p) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_b = sqrt(n_b) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))

  # print(c(lambda = lambda, lambda_p = lambda_p, lambda_b = lambda_b))

  seam_df = empirical_df

  seam_df$z =
    lambda * empirical_df$z +
    lambda_b * synth_batter_df$z +
    lambda_p * synth_pitcher_df$z

  seam_mod_df = seam_df

  seam_mod_df$z =
    lambda * empirical_df$z +
    lambda_b * empirical_batter_df$z +
    lambda_p * empirical_pitcher_df$z

  # return #####################################################################

  list(
    seam_df = seam_df, # full synthetic estimated distribution
    seam_mod_df = seam_mod_df, # seam modification with empirical for batter and pitcher
    empirical_pool = empirical_pool, # real matchup data
    empirical_df = empirical_df, # real matchup estimated distribution
    empirical_pitcher_df = empirical_pitcher_df, # real pitcher estimated distribution
    empirical_batter_df = empirical_batter_df, # real batter estimated distribution
    empirical_both_df = empirical_both_df, # 50-50 blend of real batter and real pitcher
    synth_pitcher_df = synth_pitcher_df, # synthetic pitcher estimated distribution
    synth_batter_df = synth_batter_df # synthetic batter estimated distribution
  )

}
