calc_n_synth = function(df) {
  sum(df$similarity ^ 2)
}

#' @export
do_full_seam_matchup = function(.batter, .pitcher, .bip, .batter_pool, .pitcher_pool, .ratio_batter, .ratio_pitcher) {

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

  print(pitcher_pitches)
  print(batter_pitches)

  if (sum(batter_pitches$n) <= 1) {
    status = "Not enough batter pitches."
  }

  pt_int = all(pitcher_pitches$pitch_type %in% batter_pitches$pitch_type)

  # vs all #####################################################################

  empirical_pitcher_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "pitcher"
  )

  empirical_pitcher_df = empirical_pitcher_pool %>%
    kde_helper() %>%
    kde_to_df()

  empirical_batter_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "batter"
  )

  empirical_batter_df = empirical_batter_pool %>%
    kde_helper() %>%
    kde_to_df()

  # true matchup ###############################################################

  empirical_pool = make_empirical_pool(
    .batter = .batter,
    .pitcher = .pitcher,
    .bip = .bip,
    type = "both"
  )

  n = nrow(empirical_pool)

  # DO WE REALLY WANT TO DO THIS WHEN n = 1?
  if (n == 0 | n == 1) {
    # is this trick ok?
    empirical_df = empirical_batter_df
    empirical_df$z = 0
  }

  if(n > 1) {
    empirical_df = empirical_pool %>%
      kde_helper() %>%
      kde_to_df()
  }

  # synth batter ###############################################################

  synth_batter_df = empirical_pitcher_df
  n_b = nrow(empirical_batter_pool)

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

    # print(pitcher_pitches)
    # print(synth_batter_pools)

    n_b = sum(sapply(synth_batter_pools, calc_n_synth))

    synth_batter_z = lapply(synth_batter_pools, kde_helper) %>%
      lapply(kde_to_df) %>%
      lapply(kde_z_extractor) %>%
      unlist() %>%
      matrix(ncol = length(synth_batter_pools)) %*%
      pitcher_pitches$freq_pitches %>%
      as.vector()

    synth_batter_df$z = synth_batter_z

  }

  # synth pitcher ##############################################################

  synth_pitcher_df = empirical_batter_df
  n_p = nrow(empirical_pitcher_pool)

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

    # print(batter_pitches)
    # print(synth_pitcher_pools)

    n_p = sum(sapply(synth_pitcher_pools, calc_n_synth))

    synth_pitcher_z = lapply(synth_pitcher_pools, kde_helper) %>%
      lapply(kde_to_df) %>%
      lapply(kde_z_extractor) %>%
      unlist() %>%
      matrix(ncol = length(synth_pitcher_pools)) %*%
      pitcher_pitches$freq_pitches %>%
      as.vector()

    # synth_pitcher_z2 = synth_pitcher_z
    #
    # re_weight = function(df, prop) {
    #   df$weight = df$weight * prop
    #   df
    # }
    #
    # synth_pitcher_pools = purrr::map2(synth_pitcher_pools,
    #                                   pitcher_pitches$freq_pitches,
    #                                   re_weight)
    #
    # synth_pitcher_pools = dplyr::bind_rows(synth_pitcher_pools)
    #
    # synth_pitcher_z = kde_helper(synth_pitcher_pools) %>%
    #     kde_to_df %>%
    #     kde_z_extractor
    #
    # print(data.frame(synth_pitcher_z, synth_pitcher_z2))

    # plot(synth_pitcher_z, synth_pitcher_z2)
    # abline(a = 0, b = 1, col = "dodgerblue")
    # grid()

    # print(c(sum(synth_pitcher_z * 7.040098), sum(synth_pitcher_z2 * 7.040098)))

    synth_pitcher_df$z = synth_pitcher_z

  }

  # combine ####################################################################

  print(c(n = n, n_b = n_b, n_p = n_p))

  # TODO: uh, really need to check on this!!!
  if (!pt_int) {
    temp = n_p
    n_p = n_b
    n_b = temp
  }

  lambda = sqrt(n) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_p = sqrt(n_p) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
  lambda_b = sqrt(n_b) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))

  print(c(lambda = lambda, lambda_p = lambda_p, lambda_b = lambda_b))

  seam_df = empirical_df

  seam_df$z =
    lambda * empirical_df$z +
    lambda_b * synth_batter_df$z +
    lambda_p * synth_pitcher_df$z

  # return #####################################################################

  # TODO: combine all data frames into one main df
  # TODO: x, y is the same for each, only the "z" column differs
  # TODO: instead add classes to each df that can be used for quicker plotting?
  list(
    seam_df = seam_df, # full synthetic estimated distribution
    empirical_pool = empirical_pool, # real matchup data
    empirical_df = empirical_df, # real matchup estimated distribution
    empirical_pitcher_df = empirical_pitcher_df, # real pitcher estimated distribution
    empirical_batter_df = empirical_batter_df, # real batter estimated distribution
    synth_pitcher_df = synth_pitcher_df, # synthetic pitcher estimated distribution
    synth_batter_df = synth_batter_df # synthetic batter estimated distribution
  )

}
