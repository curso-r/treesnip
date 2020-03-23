
# treesnip

<!-- badges: start -->
[![R build status](https://github.com/curso-r/treesnip/workflows/R-CMD-check/badge.svg)](https://github.com/curso-r/treesnip)
<!-- badges: end -->


## Installation

You can install the released version of treesnip from [CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("curso-r/treesnip")
```

## Example

This package provides the `tree` engine for `decision_tree` in parsnip.

``` r
library(treesnip)
library(tidymodels)

df <- data.frame(x = runif(10), y= runif(10))

mod <- decision_tree(min_n = 0, cost_complexity = 0) %>%
  set_engine("tree") %>%
  set_mode("regression")

res <- fit(mod, y ~x, df)
res
#> parsnip model object
#> 
#> Fit time:  5ms 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>   1) root 10 0.5299000 0.5917  
#>     2) x < 0.824808 8 0.3524000 0.6495  
#>       4) x < 0.187641 1 0.0000000 0.8779 *
#>       5) x > 0.187641 7 0.2927000 0.6168  
#>        10) x < 0.3321 2 0.0003889 0.3906  
#>          20) x < 0.284119 1 0.0000000 0.3767 *
#>          21) x > 0.284119 1 0.0000000 0.4046 *
#>        11) x > 0.3321 5 0.1490000 0.7073  
#>          22) x < 0.391375 1 0.0000000 0.9784 *
#>          23) x > 0.391375 4 0.0571400 0.6396  
#>            46) x < 0.447528 1 0.0000000 0.4373 *
#>            47) x > 0.447528 3 0.0025780 0.7070  
#>              94) x < 0.559171 1 0.0000000 0.7451 *
#>              95) x > 0.559171 2 0.0004043 0.6880  
#>               190) x < 0.679561 1 0.0000000 0.7022 *
#>               191) x > 0.679561 1 0.0000000 0.6737 *
#>     3) x > 0.824808 2 0.0441000 0.3607  
#>       6) x < 0.956974 1 0.0000000 0.2122 *
#>       7) x > 0.956974 1 0.0000000 0.5092 *
pred <- predict(res, df)
pred
#> # A tibble: 10 x 1
#>    .pred
#>    <dbl>
#>  1 0.674
#>  2 0.377
#>  3 0.702
#>  4 0.745
#>  5 0.405
#>  6 0.212
#>  7 0.978
#>  8 0.509
#>  9 0.878
#> 10 0.437
```

