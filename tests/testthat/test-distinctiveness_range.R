context("Test Range-based Distinctiveness")
library("dplyr")

# Initial data -----------------------------------------------------------------
# Empty Matrix
empty_mat = matrix(rep(0, 4), ncol = 2)
rownames(empty_mat) = paste0("s", 1:2)
colnames(empty_mat) = letters[1:2]

# Valid Presence-Absence Matrix
valid_mat = matrix(c(1, 0, 0, 0,
                     rep(1, 3), 0,
                     0, rep(1, 3),
                     0, 1, 0, 1),
                   ncol = 4)

dimnames(valid_mat) = list("site" = paste0("s", 1:4), "species" = letters[1:4])

# Community df
log_mat = (valid_mat == 1)

suppressWarnings({
  com_df = lapply(rownames(log_mat), function(x) {
    species = colnames(valid_mat)[log_mat[x, ]]
    data.frame(site = rep(x, length(species)), species = species)
  }) %>%
    bind_rows()
})


# Traits df
trait_df = data.frame(tr1 = c("A", "A", "B", "B"), tr2 = c(rep(0, 3), 1),
                      tr3 = seq(4, 16, 4), stringsAsFactors = TRUE)

rownames(trait_df) = letters[1:4]

# Distance Matrix
dist_mat = compute_dist_matrix(trait_df)



## Correct Distinctiveness -----------------------------------------------------
good_di_0.1 = valid_mat
good_di_0.1[valid_mat == 0] = NA

good_di_0.4 = matrix(c(1/9, 1/9, NA, NA,
                       NA,  1,   1,  1,
                       NA,  1,   1,  NA,
                       NA,  NA,  1,  1),
                     nrow = 4, ncol = 4, byrow = TRUE,
                     dimnames = list("site" = paste0("s", 1:4),
                                     "species" = letters[1:4]))

good_di_0.6 = matrix(c(1/9, 1/9, NA,  NA,
                       NA,  4/9, 4/9, 4/9,
                       NA,  4/9, 4/9, NA,
                       NA,  NA,  4/9, 4/9),
                     nrow = 4, ncol = 4, byrow = TRUE,
                     dimnames = list("site" = paste0("s", 1:4),
                                     "species" = letters[1:4]))

good_di_0.9 = matrix(c(1/9, 1/9, NA,  NA,
                       NA,  6/9, 4/9, 6/9,
                       NA,  4/9, 4/9, NA,
                       NA,  NA,  4/9, 4/9),
                     nrow = 4, ncol = 4, byrow = TRUE,
                     dimnames = list("site" = paste0("s", 1:4),
                                     "species" = letters[1:4]))

good_di_relative_max = matrix(c(1,    1, NA,   NA,
                                NA, 3/4, 1/2, 3/4,
                                NA,   1,   1,  NA,
                                NA,  NA,   1,  1),
                              nrow = 4, ncol = 4, byrow = TRUE,
                              dimnames = list("site" = paste0("s", 1:4),
                                              "species" = letters[1:4]))

## Abundances ------------------------------------------------------------------
# Define Abundance Matrix
com_df_ex = bind_cols(com_df, data.frame(abund = c(0.3, 0.7, 0.2, 0.6,
                                                   0.2, 0.5, 0.5, 0.2,
                                                   0.8)))

abund_mat = valid_mat
abund_mat[abund_mat == 1] = com_df_ex %>%
  arrange(species) %>%
  .$abund

# Define Abundance tidy table
abund_com = abund_mat %>%
  matrix_to_stack(value_col = "abund", row_to_col = "site",
                  col_to_col = "species") %>%
  filter(abund > 0, site == "s3")
abund_com$Di = c(4/9, 4/9)

# Range-dependent good distinctiveness matrices
good_di_0.1_ab = good_di_0.1
good_di_0.4_ab = matrix(c(1/30, 7/90, NA, NA,
                          NA,   1,    1,  1,
                          NA,   1,    1,  NA,
                          NA,   NA,   1,  1),
                        nrow = 4, ncol = 4, byrow = TRUE,
                        dimnames = list("site" = paste0("s", 1:4),
                                        "species" = letters[1:4]))
good_di_0.6_ab = matrix(c(1/30, 7/90, NA,   NA,
                          NA,   8/45, 4/15, 8/45,
                          NA,   2/9,  2/9,  NA,
                          NA,   NA,   4/45, 16/45),
                        nrow = 4, ncol = 4, byrow = TRUE,
                        dimnames = list("site" = paste0("s", 1:4),
                                        "species" = letters[1:4]))

good_di_0.9_ab = matrix(c(1/30, 7/90, NA,   NA,
                          NA,   1/9,  4/15, 1/9,
                          NA,   2/9,  2/9,  NA,
                          NA,   NA,   4/45, 16/45),
                        nrow = 4, ncol = 4, byrow = TRUE,
                        dimnames = list("site" = paste0("s", 1:4),
                                        "species" = letters[1:4]))



## Sparse Matrices -------------------------------------------------------------
library(Matrix)
sparse_mat = as(valid_mat, "sparseMatrix")
sparse_ab_mat = as(abund_mat, "sparseMatrix")

# Tests for Distinctiveness ----------------------------------------------------

test_that("Bad input generates error", {
  expect_error(distinctiveness_range(valid_mat, dist_mat, "a"),
               "'given_range' argument should be non-null and numeric")

  expect_error(distinctiveness_range(valid_mat, dist_mat, 0.1, relative = "A"),
               "'relative' argument should be either TRUE or FALSE")

  expect_warning(
    distinctiveness_range(valid_mat * 4, dist_mat, 0.1),
    paste0("Provided object may not contain relative abundances nor ",
           "presence-absence\n",
           "Have a look at the make_relative() function if it is the case"),
    fixed = TRUE
  )
})

test_that("Correct Di computation without abundance",{

  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness_range(valid_mat, dist_mat, 0.1)),
                   dimnames(valid_mat))

  # Check computation equal
  # When T < 1/9
  expect_equivalent(distinctiveness_range(valid_mat, dist_mat, 0.1),
                    good_di_0.1)

  # 1/9 ≤ T < 4/9
  expect_equivalent(distinctiveness_range(valid_mat, dist_mat, 0.4),
                    good_di_0.4)

  # 4/9 ≤ T < 8/9
  expect_equivalent(distinctiveness_range(valid_mat, dist_mat, 0.6),
                    good_di_0.6)

  #  T ≥ 8/9
  expect_equivalent(distinctiveness_range(valid_mat, dist_mat, 0.9),
                    good_di_0.9)

  expect_equivalent(distinctiveness_range(valid_mat, dist_mat, 4, TRUE),
                    good_di_relative_max)
})

test_that("Correct Di computation with abundance",{

  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness_range(abund_mat, dist_mat, 0.1)),
                   dimnames(abund_mat))

  # Check computation equal
  # When T < 1/9
  expect_equivalent(distinctiveness_range(abund_mat, dist_mat, 0.1),
                    good_di_0.1_ab)

  # 1/9 ≤ T < 4/9
  expect_equivalent(distinctiveness_range(abund_mat, dist_mat, 0.4),
                    good_di_0.4_ab)

  # 4/9 ≤ T < 8/9
  expect_equivalent(distinctiveness_range(abund_mat, dist_mat, 0.6),
                    good_di_0.6_ab)

  #  T ≥ 8/9
  expect_equivalent(distinctiveness_range(abund_mat, dist_mat, 0.9),
                    good_di_0.9_ab)
})

test_that("Distinctiveness works with sparse matrices", {
  expect_silent(distinctiveness_range(sparse_mat, dist_mat, 0.1))

  ## Presence-absence
  # When T < 1/9
  expect_equivalent(distinctiveness_range(sparse_mat, dist_mat, 0.1),
                    as(good_di_0.1, "dgeMatrix"))

  # 1/9 ≤ T < 4/9
  expect_equivalent(distinctiveness_range(sparse_mat, dist_mat, 0.4),
                    as(good_di_0.4, "dgeMatrix"))

  # 4/9 ≤ T < 8/9
  expect_equivalent(distinctiveness_range(sparse_mat, dist_mat, 0.6),
                    as(good_di_0.6, "dgeMatrix"))

  #  T ≥ 8/9
  expect_equivalent(distinctiveness_range(sparse_mat, dist_mat, 0.9),
                    as(good_di_0.9, "dgeMatrix"))

  ## Abundances
  # When T < 1/9
  expect_equivalent(distinctiveness_range(sparse_ab_mat, dist_mat, 0.1),
                    as(good_di_0.1_ab, "dgeMatrix"))

  # 1/9 ≤ T < 4/9
  expect_equivalent(distinctiveness_range(sparse_ab_mat, dist_mat, 0.4),
                    as(good_di_0.4_ab, "dgeMatrix"))

  # 4/9 ≤ T < 8/9
  expect_equivalent(distinctiveness_range(sparse_ab_mat, dist_mat, 0.6),
                    as(good_di_0.6_ab, "dgeMatrix"))

  #  T ≥ 8/9
  expect_equivalent(distinctiveness_range(sparse_ab_mat, dist_mat, 0.9),
                    as(good_di_0.9_ab, "dgeMatrix"))
})



