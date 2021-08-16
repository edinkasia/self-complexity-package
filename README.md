
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
install.packages("devtools")
devtools::install_github("edinkasia/self-complexity-package")
```

## Quick demo

Use the `calculate_overlap()` function to create a dataframe with
overlap scores for an example dataset:

``` r
library(selfcomplexity)
data(complexity_data, package = "selfcomplexity")
overlap <- calculate_overlap(complexity_data)
head(overlap)
#> # A tibble: 6 × 2
#> # Groups:   ResponseId [6]
#>   ResponseId        overlap_norm
#>   <chr>                    <dbl>
#> 1 R_2AZ6s7FRS4wbwdm        0.188
#> 2 R_WxFL7EQ3fjgV7Gh        0.267
#> 3 R_3QSJDixEPSxlFqp        0.349
#> 4 R_USc3yvg0dKWC2uR        0.377
#> 5 R_2eUqgEpVjAg82DR        0.773
#> 6 R_2E0RmTYNTx00JPO        0.513
```
