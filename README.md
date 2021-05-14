
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selfcomplexity

<!-- badges: start -->
<!-- badges: end -->

Literature on self-complexity uses a variety of indices to calculate how
complex, or consistent, people’s selves are. This package offers
functions that automate the calculation of the most frequently used
indices, including Overlap, H-index, etc.

## Installation

You can install the released version of selfcomplexity from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edinkasia/self-complexity-package")
```

## Quick demo

Use the `calculate_overlap()` function to create a dataframe with
overlap scores for an example dataset:

``` r
library(selfcomplexity)
data <- readr::read_csv("test_data/AllData_SelfAspects_LongFormat.csv")
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   .default = col_character(),
#>   n_subtypes = col_double(),
#>   Positive = col_double(),
#>   Representative = col_double(),
#>   Support = col_double(),
#>   Subtype16 = col_logical(),
#>   Subtype17 = col_logical(),
#>   Subtype18 = col_logical(),
#>   Subtype19 = col_logical(),
#>   Subtype20 = col_logical(),
#>   Subtype21 = col_logical(),
#>   Subtype22 = col_logical(),
#>   Subtype23 = col_logical(),
#>   Subtype24 = col_logical(),
#>   Subtype25 = col_logical(),
#>   Subtype26 = col_logical(),
#>   Subtype27 = col_logical(),
#>   Subtype28 = col_logical(),
#>   Subtype29 = col_logical(),
#>   Subtype30 = col_logical()
#> )
#> ℹ Use `spec()` for the full column specifications.
overlap <- calculate_overlap(data)
utils::View(overlap)
```
