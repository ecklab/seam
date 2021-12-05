# seam

<!-- badges: start -->
[![run-tets](https://github.com/ecklab/seam/workflows/run-tests/badge.svg)](https://github.com/ecklab/seam/actions)
<!-- badges: end -->

The goal of {seam} is to implement SEAM methodology in `R`.

## App

To use the Shiny application shipped with the package, first run the
`setup.R` script. This script will take some time to run as it will
download a large amount of data. This script only needs to be run once.

If `setup.R` has been run successfully, simply open `app.R` and the
click the “Run App” button.

## Installation

Because this package is currently in a private repo, the easiest way to
install the package is locally. After cloning the repo, use:

``` r
# install.packages("devtools")
devtools::install_local(force = TRUE)
```

## TODO

-   In general, consider quantiles of various “characteristics”
-   [Launch Angle and Exit
    Velo](https://baseballwithr.wordpress.com/2018/01/15/chance-of-hit-as-function-of-launch-angle-exit-velocity-and-spray-angle/)
-   [GeomMLBStadiums
    Package](https://github.com/bdilday/GeomMLBStadiums)
