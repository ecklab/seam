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

get_pitcher_pitches = function(.bip, .pitcher, .stands) {

  bip_ratio = .bip %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::filter(.data$stand == .stands) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(freq_pitches = .data$n / sum(.data$n)) %>%
    dplyr::filter(.data$freq_pitches > 0.03) # could change this proportion

  return(bip_ratio)

}

get_batter_pitches = function(.bip, .batter, .p_throws) {

  bip_ratio = .bip %>%
    dplyr::filter(.data$batter == .batter) %>%
    dplyr::filter(.data$p_throws == .p_throws) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(freq_pitches = .data$n / sum(.data$n)) %>%
    dplyr::filter(.data$freq_pitches > 0.03) # could change this proportion

  return(bip_ratio)

}

