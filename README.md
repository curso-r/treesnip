
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treesnip

<!-- badges: start -->

[![R build
status](https://github.com/curso-r/treesnip/workflows/R-CMD-check/badge.svg)](https://github.com/curso-r/treesnip)
<!-- badges: end -->

This package provides the following bindings for parsnip package:

  - the `tree` engine for `decision_tree`;
  - the `catboost` engine for `boost_tree`;
  - the `lightGBM` engine for `boost_tree`.

**docs**

  - [tree package
    docs](https://cran.r-project.org/web/packages/tree/tree.pdf)
  - [LightGBM docs](https://lightgbm.readthedocs.io/)
  - [Catboost docs](https://catboost.ai/docs/)

## Installation

Not on CRAN yet.

``` r
remotes::install_github("curso-r/treesnip")
```

**Hint:** for easy lightgbm installation, check the {rightgbm} package.

``` r
devtools::install_github("curso-r/rightgbm")
rightgbm::install_lightgbm()
```

## Minimal Example

``` r
# decision_tree
model <- parsnip::decision_tree()
parsnip::set_engine(model, "tree")

# boost_tree
model <- parsnip::boost_tree(mtry = 1, trees = 50)

parsnip::set_engine(model, "catboost")
parsnip::set_engine(model, "lightgbm")
```

## Roadmap

<table>

<thead>

<tr>

<th style="text-align:left;">

fun

</th>

<th style="text-align:left;">

tree

</th>

<th style="text-align:left;">

catboost

</th>

<th style="text-align:left;">

lightGBM

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

set\_fit

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

<tr>

<td style="text-align:left;">

set\_model\_arg

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

<tr>

<td style="text-align:left;">

set\_pred

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

<tr>

<td style="text-align:left;">

train

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

<tr>

<td style="text-align:left;">

predict

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

<tr>

<td style="text-align:left;">

multi\_predict

</td>

<td style="text-align:left;">

:white\_circle:

</td>

<td style="text-align:left;">

:red\_circle:

</td>

<td style="text-align:left;">

:red\_circle:

</td>

</tr>

<tr>

<td style="text-align:left;">

tests

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

</td>

</tr>

</tbody>

</table>

## Hyperparameters map

**decision\_tree()**

<table>

<thead>

<tr>

<th style="text-align:left;">

parsnip

</th>

<th style="text-align:left;">

tree

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

min\_n

</td>

<td style="text-align:left;">

minsize

</td>

</tr>

<tr>

<td style="text-align:left;">

cost\_complexity

</td>

<td style="text-align:left;">

mindev

</td>

</tr>

</tbody>

</table>

**boost\_tree()**

<table>

<thead>

<tr>

<th style="text-align:left;">

parsnip

</th>

<th style="text-align:left;">

catboost

</th>

<th style="text-align:left;">

lightGBM

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

mtry

</td>

<td style="text-align:left;">

rsm

</td>

<td style="text-align:left;">

feature\_fraction

</td>

</tr>

<tr>

<td style="text-align:left;">

trees

</td>

<td style="text-align:left;">

iterations

</td>

<td style="text-align:left;">

num\_iterations

</td>

</tr>

<tr>

<td style="text-align:left;">

min\_n

</td>

<td style="text-align:left;">

min\_data\_in\_leaf

</td>

<td style="text-align:left;">

min\_data\_in\_leaf

</td>

</tr>

<tr>

<td style="text-align:left;">

tree\_depth

</td>

<td style="text-align:left;">

depth

</td>

<td style="text-align:left;">

max\_depth

</td>

</tr>

<tr>

<td style="text-align:left;">

learn\_rate

</td>

<td style="text-align:left;">

learning\_rate

</td>

<td style="text-align:left;">

learning\_rate

</td>

</tr>

<tr>

<td style="text-align:left;">

loss\_reduction

</td>

<td style="text-align:left;">

<span style=" font-weight: bold;    color: red !important;">Not
found</span>

</td>

<td style="text-align:left;">

min\_gain\_to\_split

</td>

</tr>

<tr>

<td style="text-align:left;">

sample\_size

</td>

<td style="text-align:left;">

subsample

</td>

<td style="text-align:left;">

bagging\_fraction

</td>

</tr>

</tbody>

</table>

## Example

``` r
library(treesnip)
library(tidymodels)
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────── tidymodels 0.1.0 ──
#> ✓ broom     0.5.6      ✓ recipes   0.1.12
#> ✓ dials     0.0.7      ✓ rsample   0.0.7 
#> ✓ dplyr     1.0.0      ✓ tibble    3.0.1 
#> ✓ ggplot2   3.3.1      ✓ tune      0.1.1 
#> ✓ infer     0.5.1      ✓ workflows 0.1.1 
#> ✓ parsnip   0.1.2      ✓ yardstick 0.0.6 
#> ✓ purrr     0.3.4
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────── tidymodels_conflicts() ──
#> x purrr::discard()    masks scales::discard()
#> x dplyr::filter()     masks stats::filter()
#> x dplyr::group_rows() masks kableExtra::group_rows()
#> x dplyr::lag()        masks stats::lag()
#> x purrr::set_names()  masks magrittr::set_names()
#> x recipes::step()     masks stats::step()

set.seed(1)
df <- tibble(
  x = runif(10), 
  y = 2* x + rnorm(10, sd = 0.1)
)

mod <- decision_tree(min_n = 0, cost_complexity = 0) %>%
  set_engine("tree") %>%
  set_mode("regression") %>% 
  fit(y ~ x, df)

mod
#> parsnip model object
#> 
#> Fit time:  6ms 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 10 3.840000 1.1120  
#>    2) x < 0.472489 4 0.168700 0.4627  
#>      4) x < 0.318816 3 0.023280 0.3526  
#>        8) x < 0.131734 1 0.000000 0.2361 *
#>        9) x > 0.131734 2 0.002899 0.4109  
#>         18) x < 0.233595 1 0.000000 0.3728 *
#>         19) x > 0.233595 1 0.000000 0.4490 *
#>      5) x > 0.318816 1 0.000000 0.7930 *
#>    3) x > 0.472489 6 0.863300 1.5440  
#>      6) x < 0.779594 3 0.028200 1.1720  
#>       12) x < 0.644956 2 0.016700 1.1280  
#>         24) x < 0.600984 1 0.000000 1.2200 *
#>         25) x > 0.600984 1 0.000000 1.0370 *
#>       13) x > 0.644956 1 0.000000 1.2590 *
#>      7) x > 0.779594 3 0.002936 1.9170  
#>       14) x < 0.903299 1 0.000000 1.9480 *
#>       15) x > 0.903299 2 0.001476 1.9010  
#>         30) x < 0.926442 1 0.000000 1.8740 *
#>         31) x > 0.926442 1 0.000000 1.9280 *

df %>% 
  mutate(pred = predict(mod, df)$.pred) %>% 
  ggplot(aes(x = x)) +
  geom_point(aes( y = y)) +
  geom_step(aes(y = pred))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
