library(dplyr)
library(outlieR)

context("Functional Rarity Indices")


# Needed objects --------------------------------------------------------------

# Empty Matrix
empty_mat = matrix(rep(0, 4), ncol = 2)
colnames(empty_mat) = paste0("s", 1:2)
rownames(empty_mat) = letters[1:2]

# Valid Presence-Absence Matrix
valid_mat = matrix(c(1, 1, 0, 0,
                     0, rep(1, 3),
                     0, 1, 1, 0,
                     0, 0, 1, 1),
                   ncol = 4)

colnames(valid_mat) = paste0("s", 1:4)
rownames(valid_mat) = letters[1:4]

# Community Table
log_mat = (valid_mat == 1)
com_table = lapply(colnames(log_mat), function(x) {
  species = rownames(valid_mat)[log_mat[, x]]
  data.frame(site = rep(x, length(species)), species = species)
}) %>%
  bind_rows()

# Traits df
trait_df = data.frame(tr1 = c("A", "A", "B", "B"), tr2 = c(rep(0, 3), 1),
                       tr3 = seq(4, 16, 4))

rownames(trait_df) = letters[1:4]

# Distance Matrix
dist_mat = compute_dist_matrix(trait_df)



# Tests -----------------------------------------------------------------------

# Distinctiveness
test_that("Invalid input types do not work", {

  expect_error(single_com_dist("a", "species", NULL, "d"))
  expect_error(single_com_dist(3, "species", NULL, "d"))

  expect_error(pres_distinctiveness(empty_mat, dist_mat),
               "Community table does not have any communities")
})

#test_that("Distinctiveness correctly computed for a single commmunity",
#          )

test_that("Correct Di computation with different comm. without abundance",{

  # Final distinctiveness table for all communities
  correct_dist = structure(list(site = c("s1", "s1", "s2", "s2", "s2", "s3",
                                         "s3", "s4", "s4"),
                                species = c("a", "b", "b", "c", "d","b", "c",
                                            "c", "d"),
                                Di = c(1/9, 1/9, 6/9, 4/9, 6/9, 4/9, 4/9, 4/9,
                                       4/9)),
                           .Names = c("site", "species", "Di"),
                           row.names = c(NA, -9L), class = c("tbl_df", "tbl",
                                                             "data.frame"))

  expect_equivalent(data.frame(pres_distinctiveness(valid_mat, dist_mat)),
                    data.frame(correct_dist))

  expect_warning(distinctiveness(com_table, "species", "site", abund = NULL,
                                    dist_mat))

  c_dist = distinctiveness(com_table, "species", "site", abund = NULL, dist_mat)

  expect_equivalent(as.data.frame(c_dist), as.data.frame(correct_dist))
})


test_that("Distinctiveness is undefined for a community with a single species", {

  small_mat = matrix(c(1, 0, 0, 1), nrow = 2)
  rownames(small_mat) = letters[1:2]
  colnames(small_mat) = c("s1", "s2")

  undef_dist = data_frame(site = c("s1", "s2"), species = c("a", "b"),
                          Di = rep(NA, 2))

  expect_equivalent(pres_distinctiveness(small_mat, dist_mat), undef_dist)
})
