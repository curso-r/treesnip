
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treesnip <a href='https://curso-r.github.io/treesnip'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/curso-r/treesnip/workflows/R-CMD-check/badge.svg)](https://github.com/curso-r/treesnip)
<!-- badges: end -->

This package provides the following bindings for parsnip package:

-   the `tree` engine for `decision_tree`;
-   the `catboost` engine for `boost_tree` - only available in
    `catboost` branch. See [catboost](#catboost);
-   the `lightGBM` engine for `boost_tree`.

**docs**

-   [tree package
    docs](https://cran.r-project.org/web/packages/tree/tree.pdf)
-   [LightGBM docs](https://lightgbm.readthedocs.io/)
-   [Catboost docs](https://catboost.ai/docs/)

## Installation

Not on CRAN yet.

``` r
remotes::install_github("curso-r/treesnip")
```

See [catboost](#catboost) to use with `catboost`.

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

**decision_tree()**

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
min_n
</td>
<td style="text-align:left;">
minsize
</td>
</tr>
<tr>
<td style="text-align:left;">
cost_complexity
</td>
<td style="text-align:left;">
mindev
</td>
</tr>
</tbody>
</table>

**boost_tree()**

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
feature_fraction
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
num_iterations
</td>
</tr>
<tr>
<td style="text-align:left;">
min_n
</td>
<td style="text-align:left;">
min_data_in_leaf
</td>
<td style="text-align:left;">
min_data_in_leaf
</td>
</tr>
<tr>
<td style="text-align:left;">
tree_depth
</td>
<td style="text-align:left;">
depth
</td>
<td style="text-align:left;">
max_depth
</td>
</tr>
<tr>
<td style="text-align:left;">
learn_rate
</td>
<td style="text-align:left;">
learning_rate
</td>
<td style="text-align:left;">
learning_rate
</td>
</tr>
<tr>
<td style="text-align:left;">
loss_reduction
</td>
<td style="text-align:left;">
<span style=" font-weight: bold;    color: red !important;">Not
found</span>
</td>
<td style="text-align:left;">
min_gain_to_split
</td>
</tr>
<tr>
<td style="text-align:left;">
sample_size
</td>
<td style="text-align:left;">
subsample
</td>
<td style="text-align:left;">
bagging_fraction
</td>
</tr>
</tbody>
</table>

## Catboost

Originally `treesnip` had support for both `lightgbm` and `catboost`.
Since `catboost` has no intent to make it to CRAN we removed the parsnip
implementation from the main package. You can still use it from the
`catboost` branch that we will keep up to date with the main branch.

The `catboost` branch can be installed with:

    remotes::install_github("curso-r/treesnip@catboost")
