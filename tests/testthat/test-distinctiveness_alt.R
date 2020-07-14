# Test Range-based Alternative Distinctiveness
# Initial data -----------------------------------------------------------------
# Presence-Absence Matrix
valid_mat = matrix(1, ncol = 3, nrow = 1,
                   dimnames = list(site = "s1", species = letters[1:3]))
# Relative Abundance Matrix
abund_mat = matrix(c(1/8, 5/8, 2/8), ncol = 3, nrow = 1,
                   dimnames = list(site = "s1", species = letters[1:3]))

# Distance Matrix
dist_mat = matrix(c(0, 1/2, 1, 1/2, 0, 1/3, 1, 1/3, 0), ncol = 3, nrow = 3)
colnames(dist_mat) = letters[1:3]
rownames(dist_mat) = letters[1:3]


## Correct Distinctiveness -----------------------------------------------------

## Presence Absence
# T < 1/3
good_di_0.1 = valid_mat

# 1/3 ≤ T < 1/2
good_di_0.4 = valid_mat
good_di_0.4[] = c(1, rep(11/12, 2))

# 1/2 ≤ T < 1
good_di_0.7 = valid_mat
good_di_0.7[] = c(6/7, 25/42, 31/42)

# T ≥ 1
good_di_1.1 = valid_mat
good_di_1.1[] = c(15/22, 25/66, 20/33)


## Relative Abundances Matrix
# T < 1/3
good_di_0.1_ab = valid_mat

# 1/3 ≤ T < 1/2
good_di_0.4_ab = valid_mat
good_di_0.4_ab[] = c(1, 8/9, 31/36)

# 1/2 ≤ T < 1
good_di_0.7_ab = valid_mat
good_di_0.7_ab[] = c(39/49, 5/9, 71/126)

# T ≥ 1
good_di_1.1_ab = valid_mat
good_di_1.1_ab[] = c(45/77, 35/99, 40/99)

## Sparse Matrices -------------------------------------------------------------
library(Matrix)
sparse_mat = as(valid_mat, "sparseMatrix")
sparse_ab_mat = as(abund_mat, "sparseMatrix")

# Tests for Distinctiveness ----------------------------------------------------

test_that("Bad input generates error", {
  expect_error(distinctiveness_alt(valid_mat, dist_mat, "a"),
               "'given_range' argument should be non-null and numeric")

  expect_warning(
    distinctiveness_alt(valid_mat * 4, dist_mat, 0.1),
    paste0("Provided object may not contain relative abundances nor ",
           "presence-absence\n",
           "Have a look at the make_relative() function if it is the case"),
    fixed = TRUE
  )
})

test_that("Correct Di computation without abundance",{

  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness_alt(valid_mat, dist_mat, 0.1)),
                   dimnames(valid_mat))

  # Check computation equal
  # T < 1/3
  expect_equal(distinctiveness_alt(valid_mat, dist_mat, 1/10),
               good_di_0.1)

  # 1/3 ≤ T < 1/2
  expect_equal(distinctiveness_alt(valid_mat, dist_mat, 0.4),
               good_di_0.4)

  # 1/2 ≤ T < 1
  expect_equal(distinctiveness_alt(valid_mat, dist_mat, 0.7),
               good_di_0.7)

  # T ≥ 1
  expect_equal(distinctiveness_alt(valid_mat, dist_mat, 1.1),
               good_di_1.1)
})

test_that("Correct Di computation with abundance",{
  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness_alt(abund_mat, dist_mat, 0.1)),
                   dimnames(abund_mat))

  # Check computation equal
  # T < 1/3
  expect_equal(distinctiveness_alt(abund_mat, dist_mat, 0.1),
               good_di_0.1_ab)

  # 1/3 ≤ T < 1/2
  expect_equal(distinctiveness_alt(abund_mat, dist_mat, 4/10),
               good_di_0.4_ab)

  # 1/2 ≤ T < 1
  expect_equal(distinctiveness_alt(abund_mat, dist_mat, 7/10),
               good_di_0.7_ab)

  # T ≥ 1
  expect_equal(distinctiveness_alt(abund_mat, dist_mat, 11/10),
               good_di_1.1_ab)
})

test_that("Distinctiveness works with sparse matrices", {
  expect_silent(distinctiveness_alt(sparse_mat, dist_mat, 0.1))

  ## Presence-absence
  # Check computation equal
  # T < 1/3
  expect_equal(distinctiveness_alt(sparse_mat, dist_mat, 1/10),
               as(good_di_0.1, "dgeMatrix"))

  # 1/3 ≤ T < 1/2
  expect_equal(distinctiveness_alt(sparse_mat, dist_mat, 0.4),
               as(good_di_0.4, "dgeMatrix"))

  # 1/2 ≤ T < 1
  expect_equal(distinctiveness_alt(sparse_mat, dist_mat, 0.7),
               as(good_di_0.7, "dgeMatrix"))

  #  T ≥ 1
  expect_equal(distinctiveness_alt(sparse_mat, dist_mat, 1.1),
               as(good_di_1.1, "dgeMatrix"))

  ## Abundances
  # T < 1/3
  expect_equal(distinctiveness_alt(sparse_ab_mat, dist_mat, 0.1),
               as(good_di_0.1_ab, "dgeMatrix"))

  # 1/3 ≤ T < 1/2
  expect_equal(distinctiveness_alt(sparse_ab_mat, dist_mat, 4/10),
               as(good_di_0.4_ab, "dgeMatrix"))

  # 1/2 ≤ T < 1
  expect_equal(distinctiveness_alt(sparse_ab_mat, dist_mat, 7/10),
               as(good_di_0.7_ab, "dgeMatrix"))

  # T ≥ 1
  expect_equal(distinctiveness_alt(sparse_ab_mat, dist_mat, 11/10),
               as(good_di_1.1_ab, "dgeMatrix"))
})



