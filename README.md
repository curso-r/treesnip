
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treesnip <a href='https://curso-r.github.io/treesnip'><img src='man/figures/logo.png' align="right" height="139" /></a>

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

:heavy\_check\_mark:

</td>

<td style="text-align:left;">

:heavy\_check\_mark:

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
