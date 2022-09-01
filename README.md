# `funrar` package

<!-- badges: start -->
[![R-CMD-check](https://github.com/Rekyt/funrar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rekyt/funrar/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/Rekyt/funrar/coverage.svg?branch=master)](https://codecov.io/github/Rekyt/funrar?branch=master)
![](http://www.r-pkg.org/badges/version/funrar)
![](http://cranlogs.r-pkg.org/badges/grand-total/funrar)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.596011.svg)](https://doi.org/10.5281/zenodo.596011)
<!-- badges: end -->

`funrar` is a package to compute functional rarity indices, it quantifies how species are rare both from a functional and an extent point of view. Following the different facets of rarity proposed by Rabinowitz (1981). See this reference for more details on Functional Rarity indices:

> Violle C., Thuiller W., Mouquet N., Munoz F., Kraft NJB, Cadotte MW, Livingstone SW, Mouillot D., Functional Rarity: The Ecology of Outliers, *Trends in Ecology & Evolution*, Volume 32, Issue 5, May 2017, Pages 356-367, ISSN 0169-5347, https://doi.org/10.1016/j.tree.2017.02.002.

## Citation

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

If you want to have the latest development version use `remotes`:

```r
# install.packages("remotes") # If 'remotes' is not installed yet
remotes::install_github("Rekyt/funrar", build_vignettes = TRUE)
```


## Example vignettes

In addition to code example included in help of functions, two vignettes explain how to use the package. The 
[functional rarity indices](https://rekyt.github.io/funrar/articles/funrar.html) vignette explains 
in details the different indices and function provided; while the 
[sparse matrices](https://rekyt.github.io/funrar/articles/sparse_matrices.html) vignette shows how to use 
sparse matrices to gain speed in memory when computing functional rarity indices.

Access the vignette through R using the `vignette()` abd `browseVignettes()` functions.


## References

Rabinowitz D., Seven forms of rarity  In The Biological Aspects of Rare Plant Conservation (1981), pp. 205-217

