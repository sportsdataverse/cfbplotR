
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **cfbplotR**

<!-- badges: start -->

[![CRAN
status](https://img.shields.io/cran/v/cfbplotR?style=flat-square&logo=R&label=CRAN)](https://CRAN.R-project.org/package=cfbplotR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://img.shields.io/github/workflow/status/Kazink36/cfbplotR/R-CMD-check?label=R%20check&style=flat-square&logo=github)](https://github.com/Kazink36/cfbplotR/actions)
<!-- badges: end -->

The code for this package was copied heavily from
[nflplotR](https://nflverse.github.io/nflplotR/index.html) with minor
changes to support college football team logos.

The goal of cfbplotR is to provide functions and geoms that help
visualization of CFB related analysis. It provides a ggplot2 geom that
does the heavy lifting of plotting CFB logos in high quality, with
correct aspect ratio and possible transparency.

## Installation

<!-- You can install the released version of cfbplotR from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("cfbplotR") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->
<!-- ``` r -->
<!-- # install.packages("devtools") -->
<!-- devtools::install_github("Kazink36/cfbplotR") -->
<!-- ``` -->

You can install `nflplotR` with

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("Kazink36/cfbplotR")
```
