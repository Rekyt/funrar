# Test file for function to convert abundance matrix to relative abundances
# matrix
context("Convert Absolute and Relative Abund Matrices")

# Packages ---------------------------------------------------------------------
library(Matrix)

# Common objects ---------------------------------------------------------------

# Abundance Matrix to test
abs_abund_mat = matrix(c(1, 1, 1, NA,
                         NA, rep(1, 3),
                         NA, 1, 1, NA,
                         NA, NA, 1, 1),
                       ncol = 4)

sparse_abs_abund = as(abs_abund_mat, "sparseMatrix")

# Corresponding relative abundances matrix
rel_abund_mat = matrix(c(1,  1/3, 1/4, NA,
                         NA, 1/3, 1/4, 1/2,
                         NA, 1/3, 1/4, NA,
                         NA, NA,  1/4, 1/2),
                       ncol = 4)

sparse_rel_abund = as(rel_abund_mat, "sparseMatrix")

# Tests ------------------------------------------------------------------------

test_that("Can convert from absolute to relative abundances matrices", {
  # On regular matrices
  expect_equal(make_relative(abs_abund_mat), rel_abund_mat)

  # On sparse matrices
  expect_equal(make_relative(sparse_abs_abund), sparse_rel_abund)
})

test_that("Convert relative abundances to absolute abundances matrices", {
  # On regular matrices
  expect_equal(make_absolute(rel_abund_mat), abs_abund_mat)

  # On sparse matrices
  expect_equal(make_absolute(sparse_rel_abund), sparse_abs_abund)
})
