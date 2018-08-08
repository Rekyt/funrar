context("Test Distinctiveness")
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
                      tr3 = seq(4, 16, 4))

rownames(trait_df) = letters[1:4]

# Distance Matrix
dist_mat = compute_dist_matrix(trait_df)



## Correct Distinctiveness -----------------------------------------------------
# Final distinctiveness table for all communities
correct_dist = structure(list(site = c("s1", "s1", "s2", "s2", "s2", "s3",
                                       "s3", "s4", "s4"),
                              species = c("a", "b", "b", "c", "d","b", "c",
                                          "c", "d"),
                              Di = c(1/9, 1/9, 6/9, 4/9, 6/9, 4/9, 4/9, 4/9,
                                     4/9)),
                         .Names = c("site", "species", "Di"),
                         row.names = c(NA, -9L), class = c("tbl_df", "tbl",
                                                           "data.frame")) %>%
  # Forced to arrange by species to specify for distinctiveness matrix
  dplyr::arrange(species)

correct_dist_mat = table(correct_dist$site, correct_dist$species)

correct_dist_mat[which(correct_dist_mat == 0)] = NA_real_

correct_dist_mat[which(correct_dist_mat == 1)] = correct_dist$Di

# Distinctiveness with abundances
correct_dist_ab = correct_dist


## Undefined Di ----------------------------------------------------------------
# Undefined Distinctiveness site-species matrix
small_mat = matrix(c(1, 0, 0, 1), nrow = 2)
colnames(small_mat) = letters[1:2]
rownames(small_mat) = c("s1", "s2")

small_df = matrix_to_tidy(small_mat)

# Small tidy df with undefined Di
undef_dist = data_frame(site = c("s1", "s2"), species = c("a", "b"),
                        Di = rep(NaN, 2))

# Small matrix with undefined Di
undef_dist_mat = table(undef_dist$site, undef_dist$species)

undef_dist_mat[which(undef_dist_mat == 0)] = NA_real_

undef_dist_mat[which(undef_dist_mat == 1)] = undef_dist$Di

suppressWarnings({
  suppressMessages({
    undef_test = distinctiveness_range(small_mat, dist_mat, 0.1)
  })
})



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


## Sparse Matrices -------------------------------------------------------------
library(Matrix)
sparse_mat = as(valid_mat, "sparseMatrix")

dist_sparse_mat = as(correct_dist_mat, "sparseMatrix") %>%
  as("dgeMatrix")

# Tests for Distinctiveness ----------------------------------------------------

test_that("Bad input generates error", {
  expect_error(distinctiveness_range(valid_mat, dist_mat, "a"),
               "'given_range' argument should be non-null and numeric")
})

test_that("Correct Di computation with different comm. without abundance",{

  # expect_message(distinctiveness(valid_mat[-1, -1], dist_mat))
  #
  # expect_message(distinctiveness(valid_mat[2:3, 1:4], dist_mat[-1, -1]))

  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness_range(valid_mat, dist_mat, 0.1)),
                   dimnames(valid_mat))

  # Check computation equal
  # When T < 1.9
  expect_equivalent(valid_mat,
                    distinctiveness_range(valid_mat, dist_mat, 0.1))
})


test_that("Di is undefined for a community with a single species", {

  ## Test for matrix version of distinctiveness
  expect_equivalent(as(undef_dist_mat, "matrix"), undef_test)

  # Check warning for NaN created in the matrix
  expect_warning(distinctiveness(small_mat, dist_mat),
                 regexp = paste0("Some communities had a single species in ",
                                 "them\nComputed value assigned to 'NaN'"))
})


test_that("Distinctiveness works with sparse matrices", {
  expect_silent(distinctiveness_range(sparse_mat, dist_mat, 0.1))

  expect_equivalent(distinctiveness_range(sparse_mat, dist_mat, 0.1),
                    as(sparse_mat, "dgeMatrix"))
})



