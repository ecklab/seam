# a function to merge functionality from (and improve upon) the following:
# https://stat.ethz.ch/pipermail/r-help/2006-June/107405.html
# MASS::kde2d
# ggtern::kde2d.weighted
kde = function(x, y = NULL, h = NULL, w = NULL, n = 100, lims = c(-150, 150, -30, 200), ...) {

  if (is.null(y)) {
    return(stats::density(x, ...))
  }

  nx = length(x)

  if (length(y) != nx) {
    stop("Data vectors must be the same length.")
  }

  if (any(!is.finite(x)) || any(!is.finite(y))) {
    stop("Missing or infinite values in the data are not allowed.")
  }

  if (any(!is.finite(lims))) {
    stop("Only finite values are allowed in 'lims'.")
  }

  if (is.null(w)) {
    w = numeric(nx) + 1
  }

  if (length(w) == 1 & w[[1]] == 1) {
    w = rep(w, nx)
  }

  if (length(w) != nx) {
    stop("Weight vectors must be 1 or length of data.")
  }

  if (is.null(h)) {
    h = c(pick_bandwidth(x), pick_bandwidth(y))
  } else {
    h = rep(h, length.out = 2)
  }

  if (any(h <= 0)) {
    stop("Bandwidths must be strictly positive.")
  }

  gx = seq.int(lims[1], lims[2], length.out = n)
  gy = seq.int(lims[3], lims[4], length.out = n)
  h = h / 4
  ax = outer(gx, x, "-") / h[1]
  ay = outer(gy, y, "-") / h[2]
  wm = matrix(rep(w, n), nrow = n, ncol = nx, byrow = TRUE)
  xm = matrix(stats::dnorm(ax), n, nx)
  ym = matrix(stats::dnorm(ay), n, nx)
  z = tcrossprod(wm * xm, ym) / (sum(w) * h[1] * h[2])

  return(list(x = gx, y = gy, z = z))

}

pick_bandwidth = function(x) {
  h = diff(stats::quantile(x, c(0.25, 0.75))) / 1.34
  4.24 * min(stats::sd(x), h) * length(x) ^ (-1 / 5)
}

kde_helper = function(df) {
  kde(x = df$x, y = df$y, w = df$weight)
}

kde_z_extractor = function(kde_df) {
  kde_df$z
}

kde_to_df = function(kde) {

  df = expand.grid(
    x = kde$x,
    y = kde$y
  )

  df$z = as.numeric(kde$z)

  return(df)

}
