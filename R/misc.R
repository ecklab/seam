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
    # message("This matchup has not occurred within the balls-in-play specified.")
    p_bip = bip[bip$pitcher == p_id, ]
    p_throws = table(p_bip$p_throws)
    p_throws = names(p_throws[which.max(p_throws)])
    b_bip = bip[bip$batter == b_id & bip$p_throws == p_throws, ]
    if (nrow(b_bip) == 0) {
      # message("Batter has not faced a pitcher with the handedness of this pitcher in the balls-in-play specified.")
      # message("Using most common handedness of this batter.")
      b_bip = bip[bip$batter == b_id, ]
    } else {
      # message("Using most common handedness of this batter against a pitcher with most common handedness of this pitcher.")
    }
    b_stands = table(b_bip$stand)
    b_stands = names(b_stands[which.max(b_stands)])
    return(c(
      b_stands = b_stands,
      p_throws = p_throws
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

# this function isn't great, but makes it easier to update the stadium
get_batter_stadium = function(.b_lu, teams, .batter) {

  b_team = .b_lu %>%
    filter(.data$batter == lu_b(.b_lu = .b_lu, .batter)) %>%
    pull(team)

  teams$team[which(teams$team_abbr == b_team)]

}

re_weight = function(df, prop) {
  df$weight = df$weight * prop
  df
}
