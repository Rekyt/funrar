# `funrar` package

[![Travis-CI Build Status](https://travis-ci.org/Rekyt/funrar.svg?branch=master)](https://travis-ci.org/Rekyt/funrar) [![codecov.io](https://codecov.io/github/Rekyt/funrar/coverage.svg?branch=master)](https://codecov.io/github/Rekyt/funrar?branch=master)
![](http://www.r-pkg.org/badges/version/funrar)
![](http://cranlogs.r-pkg.org/badges/grand-total/funrar)
[![DOI](https://zenodo.org/badge/47263274.svg)](https://zenodo.org/badge/latestdoi/47263274)

`funrar` is a package to compute functional rarity indices, it quantifies how species are rare both from a functional and an extent point of view. Following the different facets of rarity proposed by Rabinowitz (1981). See this reference for more details on Functional Rarity indices:

> Violle C., Thuiller W., Mouquet N., Munoz F., Kraft NJB, Cadotte MW, Livingstone SW, Mouillot D., Functional Rarity: The Ecology of Outliers, *Trends in Ecology & Evolution*, Volume 32, Issue 5, May 2017, Pages 356-367, ISSN 0169-5347, https://doi.org/10.1016/j.tree.2017.02.002.

### Citation

**NEWS: the funrar paper just got published**

Please cite the following reference when using `funrar` in a paper:

> Grenié M, Denelle P, Tucker CM, Munoz F, Violle C. funrar: An R package to characterize functional rarity. Divers Distrib. 2017;00:1–7. https://doi.org/10.1111/ddi.12629

or refer to the [CITATION file](inst/CITATION), using:

```r
citation(package = "funrar")
```

## Installation

The package is on CRAN, you can install it using:

```r
install.packages("funrar")
```

If you want to have the latest development version use `devtools`:

```r
# install.packages("devtools") # If 'devtools' is not installed yet
devtools::install_github("Rekyt/funrar", build_vignettes = TRUE)
```

## Dependencies

Apart from base packages dependencies, `funrar` depends on `dplyr` and `cluster`.


## Example vignettes

In addition to code example included in help of functions, two vignettes explain how to use the package. The [functional rarity indices](vignettes/rarity_indices.Rmd) vignette explains in details the different indices and function provided; while the [sparse matrices](vignettes/sparse_matrices.Rmd) vignette shows how to use sparse matrices to gain speed in memory when computing functional rarity indices.

Access the vignette through R using the `vignette()` function.

## References

Rabinowitz D., Seven forms of rarity  In The Biological Aspects of Rare Plant Conservation (1981), pp. 205-217

