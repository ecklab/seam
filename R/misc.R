# these are generally more developed than functions in scratch which are being developed from scratch

get_matchup_hands = function(bip, b_id, p_id) {

  matchup_bip = bip[bip$batter == b_id & bip$pitcher == p_id, ]
  b_stands = unique(matchup_bip$stand)
  p_throws = unique(matchup_bip$p_throws)

  if (length(b_stands) > 1) {
    stop("This matchup is not currently supported because the batter switch hits
         against this pitcher.")
  }

  if (length(p_throws) > 1) {
    stop("This matchup is not currently supported because the pitcher does not
         use a consistent throwing hand against the chosen batter.")
  }

  if (length(b_stands) == 0) {
    message("This matchup has not occurred. Using most common handedness.")
    # TODO: we're not actually doing what the warning says at the moment
    # TOOD: rethink this whole function
    p_bip = bip[bip$pitcher == p_id, ]
    b_bip = bip[bip$batter == b_id, ]
    return(c(
      b_stands = unique(b_bip$stand),
      p_throws = unique(p_bip$p_throws)
    ))
  }

  return(c(b_stands = b_stands, p_throws = p_throws))

}

# TODO: pre-calculate this information for all pitchers, query within seam
get_pitcher_pitches = function(.bip, .pitches, .pitcher) {

  pitch_ratio = .pitches %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(freq_pitches = .data$n / sum(.data$n)) %>%
    dplyr::select("pitch_type", "freq_pitches")

  bip_ratio = .bip %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n())

  pitcher_ratios = dplyr::left_join(bip_ratio, pitch_ratio) %>%
    dplyr::filter(.data$freq_pitches > 0.03) # could change this proportion

  pitcher_ratios$freq_pitches = pitcher_ratios$freq_pitches / sum(pitcher_ratios$freq_pitches)

  # TODO: note that n is based on bip while freq is based on all pitches
  return(pitcher_ratios)

}
