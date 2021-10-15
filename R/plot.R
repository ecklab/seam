plot_df = function(df) {
  ggplot(df, aes(x, y, z = z)) +
    geom_contour_filled(bins = 10) +
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
        '#bd0026',
        '#800026'
      )
    ) +
    theme_void() +
    coord_fixed()
}
