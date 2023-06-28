
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/fragla/rheumtools/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/fragla/rheumtools/actions/workflows/check-standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fragla/rheumtools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/fragla/rheumtools?branch=main)
<!-- badges: end -->

# rheumtools

The goal of rheumtools is to provide a collection of functions that can
help in the analysis of clinical rheumatology data. These include
methods for calculating disease activity scores such as
`basdai_score()`, `das_28_esr_score()` and `dapsa_score()` as well as
methods for categorising disease activity and calculating response to
treatment. Other generic methods, such as `body_mass_index()` and
`percentage_response()` are also included.

## Installation

You can install the development version of rheumtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fragla/rheumtools")
```

## Example

Calculation of disease activity using DAS28-ESR.

``` r
library(rheumtools)

#DAS28-ESR
das1 <- das_28_esr_score(tjc = 6, sjc = 5, esr = 22, ptgh = 85)
das1
#> [1] 5.35

das2 <- das_28_esr_score(tjc = 1, sjc = 1, esr = 10, ptgh = 39)
das2
#> [1] 3

#Classification
das_28_classification(das1)
#> [1] "High"
das_28_classification(das2)
#> [1] "Low"

#Response
eular_ra_response(das1, das2)
#> [1] "Good"

#The functions are also vectorised so disease activity and classification could be calculated using
das <- das_28_esr_score(tjc = c(6,1), sjc = c(5,1), esr = c(22,10), ptgh = c(85,39))
das
#> [1] 5.35 3.00

das_28_classification(das)
#> [1] "High" "Low"
```

## License

This project is licensed under the MIT License - see the
[LICENSE.md](https://github.com/fragla/rheumtools/blob/main/LICENSE.md)
file for details.
