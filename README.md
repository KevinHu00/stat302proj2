<!-- badges: start -->
[![Travis build status](https://travis-ci.com/KevinHu00/stat302proj2.svg?branch=master)](https://travis-ci.com/KevinHu00/stat302proj2)
[![Codecov test coverage](https://codecov.io/gh/KevinHu00/stat302proj2/branch/master/graph/badge.svg)](https://codecov.io/gh/KevinHu00/stat302proj2?branch=master)
<!-- badges: end -->

## Installation

To download the stat302proj2 package, use the code below.

``` r
# install.packages("devtools")
devtools::install_github("KevinHu00/stat302proj2")
library(stat302proj2)
```

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):

``` r
# install.packages("devtools")
devtools::install_github("KevinHu00/stat302prj2", build_vignette = TRUE, build_opts = c())
library(stat302prj2)
# Use this to view the vignette in the stat302prj2 HTML help
help(package = "stat302prj2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302prj2")
```
