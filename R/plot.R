geom_mlb_stadium = function(stadium_ids = "generic", stadium_segments = "all", ...) {

  mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$segment, ...)

  # TODO: this is a really silly hack currently
  data = readRDS("data/stadium-paths.Rds")

  data = do.call(rbind.data.frame, lapply(stadium_ids, function(s) {
    data[(data$team == s), ]
  }))

  data = mlbam_xy_transformation(data, x = "x", y = "y", column_suffix = "")

  ggplot2::layer(geom = ggplot2::GeomPath, mapping = mapping, data = data, stat = "identity",
        position = "identity", show.legend = NA, inherit.aes = FALSE,
        params = list(na.rm = FALSE, ...))

}

mlbam_xy_transformation = function (data, x = "hc_x", y = "hc_y", column_suffix = "_") {
  data[, paste0(x, column_suffix)] = (data[, x] - 125.42)
  data[, paste0(y, column_suffix)] = (198.27 - data[, y])
  data
}

plot_df = function(df, stadium = "generic", main = "") {

  print(head(df))

  if (sum(df$z) == 0 || all(is.na(df$z))) {    # FIXME: something wrong
    p = ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, z = z)) +
      ggplot2::xlim(-150, 150) +
      ggplot2::ylim(-40, 200) +
      ggplot2::scale_fill_brewer(palette = "Greens") +
      ggplot2::theme_void() +
      ggplot2::coord_fixed() +
      ggplot2::theme(legend.position = "none",
                     plot.caption = ggplot2::element_text(hjust = 0.5, size = 12),
                     plot.margin = ggplot2::margin(t = -10, r = -50, b = 25, l = -50)
      ) +
      ggplot2::labs(caption = main) +
      geom_mlb_stadium(stadium_ids = stadium)
    return(p)
  }

  # TODO: mirror this and the validate code
  # TODO: extract function to use for both
  x_diff = (150 + 150) / 99
  y_diff = (200 + 30) / 99
  c = x_diff * y_diff

  total = sum(df$z * c)
  df$z = df$z / total

  df = df %>%
    dplyr::arrange(.data$z) %>%
    dplyr::mutate(cum_dens = cumsum(.data$z * c))

  find_cut = function(cut, df) {
    df$z[min(which(df$cum_dens > cut))]
  }

  z_breaks = c(sapply(seq(0.10, 0.90, by = 0.10), find_cut, df = df), max(df$z) + 0.01)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, z = z)) +
    ggplot2::geom_contour_filled(breaks = z_breaks) +
    ggplot2::xlim(-150, 150) +
    ggplot2::ylim(-40, 200) +
    ggplot2::scale_fill_brewer(palette = "Greens") +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none",
                   plot.caption = ggplot2::element_text(hjust = 0.5, size = 12),
                   plot.margin = ggplot2::margin(t = -10, r = -50, b = 25, l = -50)
                   ) +
    ggplot2::labs(caption = main) +
    geom_mlb_stadium(stadium_ids = stadium)
}
