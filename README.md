# `outlieR` package
[![Travis-CI Build Status](https://travis-ci.org/Rekyt/outlieR.svg?branch=master)](https://travis-ci.org/Rekyt/outlieR)

Package project to describe Functional Rarity, may be added in the future to [package cati](http://github.com/adrientaudiere/cati/)

## Installation Procedure

As **`outlieR`** is not on CRAN you can install it using R package `devtools` as follow:

```r
devtools::install_github("Rekyt/outlieR")
```

## Dependencies

`outlieR` depends on Hadley Whickam R package for data manipulation [`dplyr`](http://github.com/hadley/dplyr). And also `cluster` package.

## Included functions

For the moment only 5 functions are exported (all of them include a help file accessible using `?function_name` in R):

- `compute_dist_matrix()`  it computes a Gower's distance matrix between species from a traits matrix, trait scaling is done automatically; **WARNING:** categorical traits should be clearly defined as `ordered` or `factor` to compute the needed distance;
- `distinctiveness()` computes the distinctiveness index (local functional rarity) over a range of species, index can be weighted by species abundances, see help for syntax;
- `sparseness()` computes the sparseness index (local abundance rarity), **needs abundances**;
- `uniqueness()` computes uniqueness index (regional functional rarity) (minimum functional distance to other species).
- `agg_ind()` computes moments (mean, variance and skewness) of functional rarity metrics per community or species.

Restrictedness index is not included in the package because its computation is dataset-dependent (convex hulls, actual range, grid-based estimations, etc.).

## Example vignette

In addition to code example included in help of functions, an example vignette can help you use the above functions. [Click here](vignettes/rarity_indices.Rmd) to access the vignette


## TO DO

- [X] Make an example vignette,
- [X] Implement tests,
- [ ] Provide example dataset,
- [ ] Implement a general restrictedness computation,
- [ ] Make species as factor compatible,
- [ ] Compute distinctiveness/sparseness functions directly from site-species matrices.
