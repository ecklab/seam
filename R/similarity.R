# TODO: consider adding a constant to modify similarities and weights
calc_sim = function(char_study, char_pool, v) {
  diff = char_study - char_pool
  as.numeric(exp(-(crossprod(diff, v) %*% diff) ^ (1 / length(diff))))
}

calc_sim_batter = function(b_study_char, b_pool_char, ratio = 0.85) {

  char = c("lf_prc", "cf_prc", "rf_prc", "launch_angle", "launch_speed")
  stuff = which(char %in% c("launch_angle", "launch_speed"))
  not_stuff = which(char %in% c("lf_prc", "cf_prc", "rf_prc"))

  char_study = b_study_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.numeric()

  char_pool = b_pool_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.matrix()

  v_dim = length(char_study)
  v = diag(1, v_dim, v_dim)

  for (s in stuff) {
    v[s, s] = ratio * v_dim / length(stuff)
  }

  for (s in not_stuff) {
    v[s, s] = (1 - ratio) * v_dim / length(not_stuff)
  }

  similarity = apply(char_pool, 1, calc_sim, char_study = char_study, v = v)
  weight = similarity / sum(similarity)

  data.frame(
    similarity = similarity,
    weight = weight
  )

}

calc_sim_pitcher = function(p_study_char, p_pool_char, ratio = 0.85) {

  char = c("release_speed", "release_spin_rate", "pfx_x", "pfx_z",
           "release_pos_x", "release_pos_y", "release_pos_z")
  stuff = which(char %in% c("release_speed", "release_spin_rate", "pfx_x", "pfx_z"))
  not_stuff = which(char %in% c("release_pos_x", "release_pos_y", "release_pos_z"))

  char_study = p_study_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.numeric()

  char_pool = p_pool_char %>%
    dplyr::select(dplyr::all_of(char)) %>%
    as.matrix()

  v_dim = length(char_study)
  v = diag(1, v_dim, v_dim)

  for (s in stuff) {
    v[s, s] = ratio * v_dim / length(stuff)
  }

  for (s in not_stuff) {
    v[s, s] = (1 - ratio) * v_dim / length(not_stuff)
  }

  similarity = apply(char_pool, 1, calc_sim, char_study = char_study, v = v)
  weight = similarity / sum(similarity)

  data.frame(
    similarity = similarity,
    weight = weight
  )

}
