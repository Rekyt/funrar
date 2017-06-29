# funrar 1.2.0

* Split `rarity_dimensions()` in two more explicit functions: `uniqueness_dimensions()` and `distinctiveness_dimensions()` split corresponding tests;
* Add internal function to compute multiple functional distance matrix using a single trait table (`combination_trait_dist()`);
* `distinctiveness()` now fully conserve the dimnames of the provided site-species matrix.

# funrar 1.1.1

* Add tests for `rarity_dimensions()`;
* `rarity_dimensions()` now comprises both Uniqueness and Distinctiveness;
* Remove packages `StatMatch`, `microbenchmark` & `reshape2` from suggested packages.

# funrar 1.1.0

* Made `make_absolute()` defunct because it was based on false assumptions and would not give back matrices of relative abundances;
* Improved examples of `make_relative()`, `uniqueness()`, `distinctiveness()` to compute across single communities or regional pools;
* Add `rarity_dimensions()` function to measure the different facets of rarity according to the trait;
* Add `center` and `scale` arguments in `compute_dist_matrix()` to scale traits before computing distance, these arguments are sensitive to the specific distance metric used;
* Use markdown with `roxygen2` to generates documentation.

# funrar 1.0.3

* Corrected bug so that dense matrices can be transformed to stack data frame using ` matrix_to_stack()` (#19),
* Updated citation for Violle et al. 2017,
* Use package [`goodpractice`](https://github.com/MangoTheCat/goodpractice) to enforce better code style,
* Add `is_relative()` function to test if matrix contains relative abundances, `scarcity()` and `distinctiveness()` now warns if it is not the case (#21),
* Conditionnally use [`microbenchmark`](https://cran.r-project.org/package=microbenchmark) following CRAN advices.


# funrar 1.0.2

* Added functions to convert absolute abundance matrix to relative abundance matrix, `make_relative()` and reverse function `make_absolute()`,
* Added a `NEWS.md` file to track changes to the package.
