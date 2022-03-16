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
