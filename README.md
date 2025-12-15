
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{FAfA}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{FAfA}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
FAfA::run_app()
```

## About

You are reading the doc about version : 0.5

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-12-11 22:13:40 +03"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading FAfA
#> ── R CMD check results ─────────────────────────────────────────── FAfA 0.5 ────
#> Duration: 7.4s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependencies missing from DESCRIPTION Imports/Depends entries:
#>     'bsicons', 'bslib', 'ggplot2'
#>   
#>   Imports includes 27 non-default packages.
#>   Importing from so many packages makes the package vulnerable to any of
#>   them becoming unavailable.  Move as many as possible to Suggests and
#>   use conditionally.
#>   
#>   See section 'The DESCRIPTION file' in the 'Writing R Extensions'
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
