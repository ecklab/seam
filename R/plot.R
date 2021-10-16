# library(GeomMLBStadiums)

geom_mlb_stadium = function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                             na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, stadium_ids = NULL,
                             stadium_segments = "outfield_outer", stadium_transform_coords = FALSE,
                             ...) {
  mapping = aes(x = x, y = y, group = segment, ...)
  data = GeomMLBStadiums::MLBStadiumsPathData
  if (is.null(stadium_ids)) {
    stadium_ids = "generic"
  }
  else if ("all" %in% stadium_ids) {
    stadium_ids = unique(data$team)
  }
  else if ("all_mlb" %in% stadium_ids) {
    stadium_ids = unique(data$team)
    cc = which(stadium_ids == "generic")
    if (length(cc) > 0) {
      stadium_ids = stadium_ids[-cc]
    }
  }
  data = do.call(rbind.data.frame, lapply(stadium_ids, function(s) {
    data[(data$team == s), ]
  }))
  if ("all" %in% stadium_segments) {
  }
  else if (!is.null(stadium_segments)) {
    data = do.call(rbind.data.frame, lapply(stadium_segments,
                                            function(s) {
                                              data[(data$segment == s), ]
                                            }))
  }
  if (stadium_transform_coords) {
    data = mlbam_xy_transformation(data, x = "x", y = "y",
                                   column_suffix = "")
  }
  layer(geom = GeomPath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

mlbam_xy_transformation = function (data, x = "hc_x", y = "hc_y", column_suffix = "_") {
  data[, paste0(x, column_suffix)] = (data[, x] - 125)
  data[, paste0(y, column_suffix)] = (199 - data[, y])
  data
}

plot_df = function(df, stadium = "generic") {
  ggplot(df, aes(x, y, z = z)) +
    geom_contour_filled(
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
    scale_fill_manual(
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
    theme_void() +
    coord_fixed() +
    geom_mlb_stadium(stadium_ids = stadium,
                     stadium_segments = "all",
                     stadium_transform_coords = TRUE)
}






# c(
#   5.772951e-08,
#   3.220922e-07,
#   1.065680e-06,
#   2.569727e-06,
#   5.385921e-06,
#   1.018753e-05,
#   1.676360e-05,
#   2.502657e-05,
#   3.513386e-05,
#   1e6
# )
