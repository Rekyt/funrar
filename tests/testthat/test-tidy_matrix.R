library(dplyr)
context("Tidy Data Frame to Matrix Transformation")


# Needed objects --------------------------------------------------------------

# Valid Presence-Absence Matrix
valid_mat = matrix(c(1, 1, NA, NA,
                     NA, rep(1, 3),
                     NA, 1, 1, NA,
                     NA, NA, 1, 1),
                   ncol = 4)

colnames(valid_mat) = paste0("s", 1:4)
rownames(valid_mat) = letters[1:4]

log_mat = (valid_mat == 1 & !is.na(valid_mat))
com_table = lapply(colnames(log_mat), function(x) {
  species = rownames(valid_mat)[log_mat[, x]]
  data.frame(site = rep(x, length(species)), species = species)
}) %>%
  bind_rows()

com_table$site = factor(com_table$site)

com_table$species = factor(com_table$species)


# Abundance Matrix
abund_mat = matrix(c(0.5, 0.5, NA, NA,
                     NA, rep(0.33, 3),
                     NA, 0.5, 0.5, NA,
                     0.25, 0.25, 0.25, 0.25),
                   ncol = 4)

dimnames(abund_mat) = list("species" = letters[1:4],
                           "site" = paste0("s", 1:4))

abund_diff = (abund_mat > 0 & !is.na(abund_mat))

abund_df = lapply(colnames(abund_mat), function(x) {
  values = abund_mat[abund_diff[, x], x]
  cbind(site = rep(x, length(values)), data.frame(species = names(values),
                                                  val = values))
  }) %>%
  bind_rows()

abund_df$species = as.factor(abund_df$species)

abund_df$site = as.factor(abund_df$site)


# Object with an NA value
na_df = abund_df
na_df[11, 3] = NA

na_mat = abund_mat
na_mat["d", "s4"] = NA

# Tests ------------------------------------------------------------------------

test_that("Conversion from tidy data frame to matrix works", {

  expect_equivalent(tidy_to_matrix(com_table, "species", "site"), valid_mat)
  expect_equivalent(tidy_to_matrix(abund_df, "species", "site", "val"), abund_mat)

  expect_error(tidy_to_matrix(com_table, "speies", "site"),
               label = "Column 'speies' is not in data.frame")

  expect_error(tidy_to_matrix(com_table, "speies", "seit"),
               label = "Columns 'speies' and 'seit' are not in data.frame")

})


test_that("Conversion from matrix to tidy data.frame works", {

  expect_equivalent(matrix_to_tidy(valid_mat, row_to_col = "species",
                                   col_to_col = "site")[, -3], com_table)

  expect_equivalent(matrix_to_tidy(abund_mat, value_col = "val"), abund_df)

  expect_equal(matrix_to_tidy(valid_mat, row_to_col = NULL,
                              col_to_col = "site") %>%
                 colnames() %>%
                 .[2],
               "row")

  expect_equal(matrix_to_tidy(valid_mat, row_to_col = "species",
                              col_to_col = NULL) %>%
                 colnames() %>%
                 .[1],
               "col")
})
