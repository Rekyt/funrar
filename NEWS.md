# funrar 1.4.0

## NEW FEATURES

* Implement `distinctiveness_global()` to compute occurrence based distinctiveness at the global/regional level using only a distance matrix

## BUG FIX

* Fix tests that used `data.frame()` now with explicit `stringAsFactors` values for compatibility with R 4.0.0
* Add missing tests on bad inputs for some functions

# funrar 1.3.1

## BUG FIX

* Test explicitly for matrix type in `rarity_dimensions()` for compatibility with R 4.0.0

# funrar 1.3.0

## NEW FEATURES

* Add `_tidy()` aliases for each `_stack()` function for easier reading (fix #29).
* `distinctiveness_stack()` returns `NA` as distinctiveness values for absent species (#30).
* New definition of functional distinctiveness based on range see `distinctiveness_range()`.
* New definition of functional distinctiveness based on range with different formula see `distinctiveness_alt()`.

## BUG FIX

* Warning when distinctiveness equals `NaN` in `distinctiveness_stack()` (#30).

# funrar 1.2.2

* Fix a bug in the test of `distinctiveness_dimensions()` that generated errors on cran server.

# funrar 1.2.1

* Add Authors' ORCID and all contributors;
* funrar paper is now included in DESCRIPTION, README.md and has a proper CITATION file;
* Fix typos in documentation;
* Transformation from tidy data.frame to sparse matrix is now possible using `stack_to_matrix(x, sparse = TRUE)` (#25);
* Add a warning message when using only continuous traits with function `comput_dist_matrix()`, as it defaults to Gower's distance (#27);
* Specification in help that functional distances need to be scaled between 0 and 1 prior to distinctiveness computation (#26).

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
* Conditional use [`microbenchmark`](https://cran.r-project.org/package=microbenchmark) following CRAN advice.


# funrar 1.0.2

* Added functions to convert absolute abundance matrix to relative abundance matrix, `make_relative()` and reverse function `make_absolute()`,
* Added a `NEWS.md` file to track changes to the package.
