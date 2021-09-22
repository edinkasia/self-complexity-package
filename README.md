
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selfcomplexity

<!-- badges: start -->

[![R-CMD-check](https://github.com/edinkasia/self-complexity-package/workflows/R-CMD-check/badge.svg)](https://github.com/edinkasia/self-complexity-package/actions)
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
data(minimal_example, package = "selfcomplexity")
overlap <- calculate_overlap(data = minimal_example, att_column = "Attributes",
                             id_column = "ResponseId", subtype_column = "Subtype_name",
                             na_name_rm = TRUE)
head(overlap)
#> # A tibble: 1 × 2
#> # Groups:   ResponseId [1]
#>   ResponseId        overlap_norm
#>   <chr>                    <dbl>
#> 1 R_WxFL7EQ3fjgV7Gh        0.267
```
