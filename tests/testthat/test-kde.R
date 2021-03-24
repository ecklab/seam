x = rnorm(10)
y = rnorm(10)
w = runif(10)
w = w / sum(w)

test_that("supplying only x calls univariate density() function", {
  expect_identical(kde(x = x), stats::density(x))
})

test_that("dots are properly passed for univariate density", {
  expect_identical(kde(x = x, kernel = "rectangular"),
                   stats::density(x, kernel = "rectangular"))
})

# # interactive only checks that kde() and pick_bandwidth() are suitable drop-in
# # replacements for some existing functions from other packages
# identical(kde(x = x, y = y, w = 1), MASS::kde2d(x = x, y = y))
# identical(kde(x = x, y = y, w = w), ggtern::kde2d.weighted(x = x, y = y, w = w))
# identical(kde(x = x), density(x))
# identical(pick_bandwidth(x), MASS::bandwidth.nrd(x))

# set.seed(42)
#
# x = rnorm(6)
# y = rnorm(6)
# w = runif(6)
# w_post = c(0.5, 0.25, 0.25)
# w_pre  = c(0.25, 0.25, 0.125, 0.125, 0.125, 0.125)
#
# range(x)
# range(y)
#
# a1 = kde(x = x[1:2], y = y[1:2], lims = c(-3, 3, -3, 3), h = 1)
# a2 = kde(x = x[3:4], y = y[3:4], lims = c(-3, 3, -3, 3), h = 1)
# a3 = kde(x = x[5:6], y = y[5:6], lims = c(-3, 3, -3, 3), h = 1)
# b = kde(x = x, y = y, lims = c(-3, 3, -3, 3), w = w_pre, h = 1)
# all.equal(0.50 * a1$z + 0.25 * a2$z + 0.25 * a3$z, b$z)
#
#
# all.equal(
#   kde(x = x, y = y, w = w),
#   kde(x = x, y = y, w = w * 100)
# )
