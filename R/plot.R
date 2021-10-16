geom_mlb_stadium = function(stadium_ids = "generic", stadium_segments = "all", ...) {

  mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$segment, ...)
  data = GeomMLBStadiums::MLBStadiumsPathData

  data = do.call(rbind.data.frame, lapply(stadium_ids, function(s) {
    data[(data$team == s), ]
  }))

  data = mlbam_xy_transformation(data, x = "x", y = "y", column_suffix = "")

  ggplot2::layer(geom = ggplot2::GeomPath, mapping = mapping, data = data, stat = "identity",
        position = "identity", show.legend = NA, inherit.aes = FALSE,
        params = list(na.rm = FALSE, ...))

}

mlbam_xy_transformation = function (data, x = "hc_x", y = "hc_y", column_suffix = "_") {
  data[, paste0(x, column_suffix)] = (data[, x] - 125)
  data[, paste0(y, column_suffix)] = (199 - data[, y])
  data
}

plot_df = function(df, stadium = "generic", pitcher, batter, main) {
  ggplot2::ggplot(df, ggplot2::aes(x = df$x, y = df$y, z = df$z)) +
    ggplot2::geom_contour_filled(
      breaks = c(
        0,
        1.068164e-05,
        2.136324e-05,
        3.204483e-05,
        4.272642e-05,
        5.340801e-05,
        6.408960e-05,
        7.477119e-05,
        8.545279e-05,
        1e6
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        '#FFFFFF',
        '#FFFFCC',
        '#ffeda0',
        '#fed976',
        '#feb24c',
        '#fd8d3c',
        '#fc4e2a',
        '#e31a1c',
        '#bd0026'
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(title = main,
         subtitle = paste(batter, "versus", pitcher)) +
    geom_mlb_stadium(stadium_ids = stadium)
}
