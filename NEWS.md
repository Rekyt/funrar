# funrar 1.0.3

* Corrected bug so that dense matrices can be transformed to stack data frame using ` matrix_to_stack()` (#19),
* Updated citation for Violle et al. 2017,
* Use package [`goodpractice`](https://github.com/MangoTheCat/goodpractice) to enforce better code style,
* Add `is_relative()` function to test if matrix contains relative abundances, `scarcity()` and `distinctiveness()` now warns if it is not the case (#21),
* Conditionnally use [`microbenchmark`](https://cran.r-project.org/package=microbenchmark) following CRAN advices.


# funrar 1.0.2

* Added functions to convert absolute abundance matrix to relative abundance matrix, `make_relative()` and reverse function `make_absolute()`,
* Added a `NEWS.md` file to track changes to the package.
