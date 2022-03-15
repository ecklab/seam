x = rnorm(10)
y = rnorm(10)
w = runif(10)
w = w / sum(w)

test_that("supplying only x to the kde() function calls univariate stats::density() function", {
  expect_identical(kde(x = x), stats::density(x))
})

test_that("kde() function properly passes dots for univariate density", {
  expect_identical(kde(x = x, kernel = "rectangular"),
                   stats::density(x, kernel = "rectangular"))
})

test_that("kde() function with weights of 1 is equivalent to the MASS::kde2d() function", {
  expect_equal(kde(x = x, y = y, w = 1),
               MASS::kde2d(x = x, y = y, n = formals(kde)$n, lims = eval(formals(kde)$lims)))
})

# test_that("kde() function with weights is equivalent to the ggtern::kde2d.weighted() function", {
#   expect_equal(kde(x = x, y = y, w = w),
#                ggtern::kde2d.weighted(x = x, y = y, w = w, n = formals(kde)$n, lims = eval(formals(kde)$lims)))
# })
