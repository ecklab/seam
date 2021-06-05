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

# TODO: for some reason this is current hard-coded as verlander
# TODO: pre-calculate this information for all pitchers, query within seam
get_pitcher_pitches = function(.bip, .pitches, .pitcher) {

  pitch_ratio = .pitches %>%
    dplyr::select("events", "pitcher", "pitch_type") %>%
    dplyr::filter(.data$pitch_type != "") %>%
    dplyr::filter(.data$pitch_type != "KN") %>%
    dplyr::filter(.data$pitch_type != "EP") %>%
    dplyr::filter(.data$pitch_type != "SC") %>%
    dplyr::filter(.data$pitch_type != "IN") %>%
    dplyr::filter(.data$pitch_type != "PO") %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "CU" = "KC")) %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FF" = "FA")) %>%
    dplyr::mutate(pitch_type = forcats::fct_recode(.data$pitch_type, "FS" = "FO")) %>%
    dplyr::filter(.data$pitcher == lu_p(p_lu, "Justin Verlander")) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(freq_pitches = .data$n / sum(.data$n)) %>%  # this needs to be based on pitches instead
    dplyr::select("pitch_type", "freq_pitches")

  bip_ratio = .bip %>%
    dplyr::filter(.data$pitcher == .pitcher) %>%
    dplyr::group_by(.data$pitch_type) %>%
    dplyr::summarise(n = dplyr::n())

  pitcher_ratios = dplyr::left_join(bip_ratio, pitch_ratio) %>%
    dplyr::filter(.data$freq_pitches > 0.02) # could change this proportion

  pitcher_ratios$freq_pitches = pitcher_ratios$freq_pitches / sum(pitcher_ratios$freq_pitches)

  return(pitcher_ratios)

}
