# these are functions that don't yet have a home or are holdovers from a previous refactor
# these are generally more developed that functions in scratch which are being developed from scratch
get_matchup_hands = function(bip, b_id, p_id) {

  matchup_bip = bip[bip$batter == b_id & bip$pitcher == p_id, ]
  b_stands = unique(matchup_bip$stand)
  p_throws = unique(matchup_bip$p_throws)

  if (length(b_stands) > 1) {
    warning("This matchup is not currently supported because the batter switch
            hits against this pitcher.")
  }

  if (length(p_throws) > 1) {
    warning("This matchup is not currently supported because the pitcher does
            not use a consistent throwing hand against the chosen batter.")
  }

  if (length(b_stands) == 0) {
    warning("This matchup has not occured in the supplied balls in play.")
  }

  return(c(b_stands = b_stands, p_throws = p_throws))

}

# get the pitches and their proportions for a pitcher
# should this be based on BIP or pitches?
get_pitcher_pitches = function(.bip, .pitcher) {
  .bip %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(freq = .data$n / sum(.data$n)) # this needs to be based on pitches instead
}
