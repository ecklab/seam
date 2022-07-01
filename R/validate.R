check_in_hdrs = function(alpha, pitch, synthetic, plot = FALSE) {

  if (plot) {
    p = plot_df(df = synthetic, main = "", stadium = "generic")
    print(p + geom_point(data = pitch, mapping = aes(x = x, y = y, z = NULL), color = "red"))
  }

  # TODO: pull these programatically from the input df? (slightly slower)
  # TODO: mirror this in plotting code
  x_diff = (150 + 150) / 99
  y_diff = (200 + 30) / 99

  c = x_diff * y_diff

  total_dens = sum(synthetic$z * c)
  cut = (1 - alpha)

  dens_synth = synthetic %>%
    dplyr::mutate(z = z / total_dens) %>%
    dplyr::arrange(z) %>%
    dplyr::mutate(cumz = cumsum(z * c)) %>%
    dplyr::arrange(desc(cumz))

  x_pos = which.min(abs(pitch$x - dens_synth$x))
  y_pos = which.min(abs(pitch$y - dens_synth$y))

  probs = dens_synth %>%
    dplyr::filter(.data$x == dens_synth$x[x_pos]) %>%
    dplyr::filter(.data$y == dens_synth$y[y_pos]) %>%
    dplyr::pull(.data$cumz)

  output = ifelse(probs > cut, TRUE, FALSE)
  names(output) = alpha
  return(output)

}

check_in_top_n = function(n, pitch, synthetic, plot = FALSE) {

  if (plot) {
    p = plot_df(df = synthetic, main = "", stadium = "generic")
    print(p + geom_point(data = pitch, mapping = aes(x = x, y = y, z = NULL), color = "red"))
  }

  # TODO: pull these programatically from the input df? (slightly slower)
  # TODO: mirror this in plotting code
  x_diff = (150 + 150) / 99
  y_diff = (200 + 30) / 99

  c = x_diff * y_diff
  total_dens = sum(synthetic$z * c)

  dens_synth = synthetic %>%
    dplyr::mutate(z = z / total_dens) %>%
    dplyr::arrange(desc(z))

  dens_synth$top_n = c(rep(TRUE, n), rep(FALSE, nrow(dens_synth) - n))

  x_pos = which.min(abs(pitch$x - dens_synth$x))
  y_pos = which.min(abs(pitch$y - dens_synth$y))

  in_top = dens_synth %>%
    dplyr::filter(.data$x == dens_synth$x[x_pos]) %>%
    dplyr::filter(.data$y == dens_synth$y[y_pos]) %>%
    dplyr::pull(.data$top_n)

  output = in_top
  names(output) = n
  return(output)

}

calc_hdr_size = function(alpha, synthetic, plot = FALSE) {

  if (plot) {
    p = plot_df(df = synthetic, main = "", stadium = "generic")
    print(p)
  }

  # TODO: pull these programatically from the input df? (slightly slower)
  # TODO: mirror this in plotting code
  x_diff = (150 + 150) / 99
  y_diff = (200 + 30) / 99

  c = x_diff * y_diff

  total_dens = sum(synthetic$z * c)
  cut = (1 - alpha)

  dens_synth = synthetic %>%
    dplyr::mutate(z = z / total_dens) %>%
    dplyr::arrange(z) %>%
    dplyr::mutate(cumz = cumsum(z * c)) %>%
    dplyr::arrange(desc(cumz))

  return(mean(dens_synth$cumz > cut))

}

calc_area_dens = function(n, synthetic, plot = FALSE) {

  if (plot) {
    p = plot_df(df = synthetic, main = "", stadium = "generic")
    print(p)
  }

  # TODO: pull these programatically from the input df? (slightly slower)
  # TODO: mirror this in plotting code
  x_diff = (150 + 150) / 99
  y_diff = (200 + 30) / 99

  c = x_diff * y_diff

  total_dens = sum(synthetic$z * c)

  z = synthetic %>%
    dplyr::mutate(z = z / total_dens) %>%
    dplyr::pull(z)

  z = sort(z, decreasing = TRUE)

  return(sum(z[1:n]))

}
