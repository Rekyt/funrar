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
                      tr3 = seq(4, 16, 4), stringsAsFactors = TRUE)

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
undef_dist = tibble(site = c("s1", "s2"), species = c("a", "b"),
                        Di = rep(NaN, 2))

# Small matrix with undefined Di
undef_dist_mat = table(undef_dist$site, undef_dist$species)

undef_dist_mat[which(undef_dist_mat == 0)] = NA_real_

undef_dist_mat[which(undef_dist_mat == 1)] = undef_dist$Di

suppressWarnings({
  suppressMessages({
    undef_test = distinctiveness(small_mat, dist_mat)
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


## Relative distinctiveness ----------------------------------------------------
# Define relative distinctiveness matrix
correct_rel_di = correct_dist_mat
correct_rel_di[1,] = correct_rel_di[1,] / (1/9)
correct_rel_di[2,] = correct_rel_di[2,] / (8/9)
correct_rel_di[3,] = correct_rel_di[3,] / (4/9)
correct_rel_di[4,] = correct_rel_di[4,] / (4/9)

correct_rel_di = as.matrix(as.data.frame.matrix(correct_rel_di))

# Relative Di df
correct_rel_di_df = structure(list(site = c("s1", "s1", "s2", "s2", "s2", "s3",
                                            "s3", "s4", "s4"),
                                   species = c("a", "b", "b", "c", "d","b", "c",
                                               "c", "d"),
                                   Di = c(1, 1, 3/4, 1/2, 3/4, 1, 1, 1, 1)),
                              .Names = c("site", "species", "Di"),
                              row.names = c(NA, -9L), class = c("data.frame"))

# Relative Di sparse
dist_sparse_mat_rel = dist_sparse_mat
dist_sparse_mat_rel[1,] = dist_sparse_mat_rel[1,] / (1/9)
dist_sparse_mat_rel[2,] = dist_sparse_mat_rel[2,] / (8/9)
dist_sparse_mat_rel[3,] = dist_sparse_mat_rel[3,] / (4/9)
dist_sparse_mat_rel[4,] = dist_sparse_mat_rel[4,] / (4/9)

# Tests for Distinctiveness ----------------------------------------------------

test_that("Invalid input types do not work", {

  expect_error(distinctiveness_com("a", "species", NULL, "d"))
  expect_error(distinctiveness_com(3, "species", NULL, "d"))

})


test_that("Correct Di computation with different comm. without abundance",{

  # Good messages and warnings
  expect_message(distinctiveness_stack(com_df, "species", "site",
                                       abund = NULL, dist_mat))

  expect_message(distinctiveness(valid_mat[-1, -1], dist_mat))

  expect_message(distinctiveness(valid_mat[2:3, 1:4], dist_mat[-1, -1]))

  # Conservation of dimensions names of indices matrix
  expect_identical(dimnames(distinctiveness(valid_mat, dist_mat)),
                   dimnames(valid_mat))

  # Good Distinctiveness computations without abundances
  c_dist = distinctiveness_stack(com_df, "species", "site", abund = NULL,
                                 dist_mat)

  expect_equivalent(correct_dist_mat,
                    as.table(distinctiveness(valid_mat, dist_mat)))

  expect_equivalent(as.data.frame(c_dist), as.data.frame(correct_dist) %>%
                      arrange(site))

  # Undefined distinctiveness for species alone in communities
  expect_equal(distinctiveness_com(com_df[1,], "species",
                                   dist_matrix = dist_mat)[1,3], NaN)


  # Distinctiveness with abundances
  expect_equal(distinctiveness_com(abund_com[, -4], "species", "abund",
                                   dist_mat), abund_com)

})


test_that("Di is undefined for a community with a single species", {

  ## Test for matrix version of distinctiveness
  expect_equivalent(as(undef_dist_mat, "matrix"), undef_test)

  # Check warning for NaN created in the matrix
  expect_warning(distinctiveness(small_mat, dist_mat),
                 regexp = paste0("Some communities had a single species in ",
                                 "them\nComputed value assigned to 'NaN'"))

  ## Test for data.frame version of distinctiveness
  expect_warning(distinctiveness_stack(small_df, "col", "row",
                                       "value", dist_mat),
                 regexp = paste0("Some communities had a single species in ",
                                 "them\nComputed value assigned to 'NaN'"))

  expect_equal(
    suppressWarnings(
      distinctiveness_stack(undef_dist[, 1:2], "species", "site",
                            dist_matrix = dist_mat)),
    undef_dist
  )

  expect_equal(
    suppressWarnings(
      distinctiveness_stack(small_df, "col", "row", "value", dist_mat)),
    data.frame(col   = rep(c("a", "b"), 2),
               row   = rep(c("s1", "s2"), each = 2),
               value = c(1, 0, 0, 1),
               Di    = c(NaN, NA, NA, NaN),
               stringsAsFactors = FALSE))
})


test_that("Distinctiveness works with sparse matrices", {
  expect_silent(distinctiveness(sparse_mat, dist_mat))

  expect_equivalent(distinctiveness(sparse_mat, dist_mat), dist_sparse_mat)
})


# Test for relative distinctiveness --------------------------------------------

test_that("Relative distinctiveness argument should be logical", {

  rel_error = "'relative' argument should be either TRUE or FALSE"

  # Character argument
  expect_error(distinctiveness(valid_mat, dist_mat, relative = "a"),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = "a"),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = "a"),
               rel_error)

  # Numeric argument
  expect_error(distinctiveness(valid_mat, dist_mat, relative = 12),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = 12),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = 12),
               rel_error)
  expect_error(distinctiveness(valid_mat, dist_mat, relative = 1),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = 1),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = 1),
               rel_error)
  expect_error(distinctiveness(valid_mat, dist_mat, relative = 0),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = 0),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = 0),
               rel_error)

  # NA argument
  expect_error(distinctiveness(valid_mat, dist_mat, relative = NA),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = NA),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = NA),
               rel_error)

  # NaN argument
  expect_error(distinctiveness(valid_mat, dist_mat, relative = NaN),
               rel_error)
  expect_error(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = NaN),
               rel_error)
  expect_error(distinctiveness_com(com_df, "species", "site",
                                   dist_matrix = dist_mat, relative = NaN),
               rel_error)


  # Logical argument
  expect_silent(distinctiveness(valid_mat, dist_mat, relative = TRUE))
  expect_silent(suppressMessages(distinctiveness_tidy(com_df, "species", "site",
                                     dist_matrix = dist_mat, relative = TRUE)))
  expect_silent(distinctiveness_com(com_df[1:2,], "species",
                                    dist_matrix = dist_mat, relative = TRUE))

  expect_silent(distinctiveness(valid_mat, dist_mat, relative = FALSE))
  expect_silent(suppressMessages(distinctiveness_tidy(com_df, "species", "site",
                                     dist_matrix = dist_mat, relative = FALSE)))

  expect_silent(distinctiveness_com(com_df[1:2,], "species",
                                    dist_matrix = dist_mat, relative = FALSE))

})

test_that("Relative distinctiveness can be computed", {

  ## Only presence-absences
  # Matrix
  expect_equivalent(distinctiveness(valid_mat, dist_mat, relative = TRUE),
                    correct_rel_di)
  # Df
  expect_equal(distinctiveness_tidy(com_df, "species", "site",
                                    dist_matrix = dist_mat, relative = TRUE),
               correct_rel_di_df)
  # Sparse mat
  expect_equivalent(distinctiveness(sparse_mat, dist_mat, relative = TRUE),
                    dist_sparse_mat_rel)

  # Single community
  expect_equivalent(distinctiveness_com(com_df[1:2, ], "species",
                                   dist_matrix = dist_mat, relative = TRUE),
                    correct_rel_di_df[1:2, ])

  ## With abundances
  # Matrix

  # Df

  # Sparse mat
})



