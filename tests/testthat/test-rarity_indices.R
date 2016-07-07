library(dplyr)
context("Functional Rarity Indices")


# General data -----------------------------------------------------------------

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

rownames(valid_mat) = paste0("s", 1:4)
colnames(valid_mat) = letters[1:4]

# Community Table
log_mat = (valid_mat == 1)


com_table = lapply(rownames(log_mat), function(x) {
  species = colnames(valid_mat)[log_mat[x, ]]
  data.frame(site = rep(x, length(species)), species = species)
}) %>%
  bind_rows()

# Traits df
trait_df = data.frame(tr1 = c("A", "A", "B", "B"), tr2 = c(rep(0, 3), 1),
                       tr3 = seq(4, 16, 4))

rownames(trait_df) = letters[1:4]

# Distance Matrix
dist_mat = compute_dist_matrix(trait_df)


# Distinctiveness data --------------------------------------------------------

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
  arrange(species)

correct_dist_mat = table(correct_dist$site, correct_dist$species)

correct_dist_mat[which(correct_dist_mat == 0)] = NA_real_

correct_dist_mat[which(correct_dist_mat == 1)] = correct_dist$Di

# Distinctiveness with abundances
correct_dist_ab = correct_dist


# Undefined Distinctiveness
small_mat = matrix(c(1, 0, 0, 1), nrow = 2)
colnames(small_mat) = letters[1:2]
rownames(small_mat) = c("s1", "s2")

undef_dist = data_frame(site = c("s1", "s2"), species = c("a", "b"),
                        Di = rep(NaN, 2))

undef_dist_mat = table(undef_dist$site, undef_dist$species)

undef_dist_mat[which(undef_dist_mat == 0)] = NA_real_

undef_dist_mat[which(undef_dist_mat == 1)] = undef_dist$Di

undef_test = distinctiveness(small_mat, dist_mat)


# Scarcity data ----------------------------------------------------------------
com_table_ex = bind_cols(com_table, data.frame(abund = c(0.3, 0.7, 0.2, 0.6,
                                                         0.2, 0.5, 0.5, 0.2,
                                                         0.8)))
abund_mat = valid_mat
abund_mat[abund_mat == 1] = com_table_ex %>%
  arrange(species) %>%
  .$abund

scarcity_mat = apply(abund_mat, 1, function(x) {
  ifelse(x != 0, exp(-sum(x != 0)*log(2)*x), NA)
}) %>% t()

com_scarcity = com_table_ex %>%
  group_by(site) %>%
  summarise(N_sp = n()) %>%
  right_join(com_table_ex, by = "site") %>%
  mutate(Si = exp(-N_sp*log(2)*abund)) %>%
  select(-N_sp)


# Test for Distinctiveness ----------------------------------------------------

test_that("Invalid input types do not work", {

  expect_error(single_com_dist("a", "species", NULL, "d"))
  expect_error(single_com_dist(3, "species", NULL, "d"))

})


test_that("Correct Di computation with different comm. without abundance",{


  # Good messages and warnings
  expect_message(table_distinctiveness(com_table, "species", "site",
                                       abund = NULL, dist_mat))

  expect_message(distinctiveness(valid_mat[-1, -1], dist_mat))

  expect_message(distinctiveness(valid_mat[2:3, 1:4], dist_mat[-1, -1]))


  # Good Distinctiveness computations without abundances
  c_dist = table_distinctiveness(com_table, "species", "site", abund = NULL,
                                 dist_mat)

  expect_equivalent(correct_dist_mat,
                    as.table(distinctiveness(valid_mat, dist_mat)))

  expect_equivalent(as.data.frame(c_dist), as.data.frame(correct_dist) %>%
                      arrange(site))


  # Distinctiveness with abundances
})


test_that("Distinctiveness is undefined for a community with a single species", {



  expect_equivalent(t(undef_dist_mat), as.table(undef_test))

  # Check warning for NaN created in the matrix
  expect_warning(distinctiveness(small_mat, dist_mat))
})


test_that("Distinctiveness works with sparse matrices", {
  library(Matrix)
  sparse_mat = as(valid_mat, "sparseMatrix")

  dist_sparse_mat = as(correct_dist_mat, "sparseMatrix") %>%
    as("dgeMatrix")

  expect_silent(distinctiveness(sparse_mat, dist_mat))

  expect_equivalent(distinctiveness(sparse_mat, dist_mat), dist_sparse_mat)
})

# Test for Uniqueness ---------------------------------------------------------

test_that("Correct Uniqueness computation", {

  valid_ui = data.frame(species = c("a", "b"), Ui = c(1/9, 1/9))

  all_ui = data.frame(species = letters[1:4],
                      Ui = c(1/9, 1/9, 4/9, 4/9))

  expect_equivalent(table_uniqueness(com_table[1:2, ], "species", dist_mat),
                    valid_ui)

  expect_error(table_uniqueness(com_table[1:2, ], "NOT_IN_TABLE", dist_mat),
    regexp = "'NOT_IN_TABLE' species column not in column names")

  expect_error(table_uniqueness(com_table[1:2, ], "species", dist_mat[1:2,]),
               regexp = "Distance matrix is not square.")

  expect_equivalent(table_uniqueness(com_table, "species", dist_mat), all_ui)

  expect_equal(uniqueness(valid_mat, dist_mat), all_ui)
})



# Test for Scarcity ------------------------------------------------------------


test_that("Correct Scarcity computation", {

  # Single community scarcity correct computation
  expect_equal(filter(com_scarcity, site == "s1"),
               single_com_scar(com_table_ex %>%
                                 filter(site == "s1") %>%
                                 as.data.frame(),
                               "species", "abund"))

  # Scarcity correct computation over many communities
  expect_equal(com_scarcity, table_scarcity(as.data.frame(com_table_ex),
                                            "species", "site", "abund"))

  # Correct Sparseness computation for a site-species matrix
  expect_equal(scarcity_mat, scarcity(abund_mat))
})


test_that("Scarcity errors with bad input", {
  expect_error(table_scarcity(as.data.frame(com_table_ex),
                              "species", "SITE_NOT_IN_TABLE", "abund"),
               regexp = "Community table does not have any communities")

  expect_error(table_scarcity(as.data.frame(com_table_ex),
                              "SPECIES_NOT_IN_TABLE", "site", "abund"),
               regexp = "Community table does not have any species")


  com_table_sp = com_table_ex

  com_table_sp$species = as.factor(com_table_ex$species)

  expect_error(table_scarcity(as.data.frame(com_table_sp),
                "species", "site", "abund"),
               regexp = "Provided species are not character")

  expect_error(table_scarcity(as.data.frame(com_table_ex), "species", "site",
                              NULL),
               regexp = "No relative abundance provided.")

  com_table_ab = com_table_ex

  com_table_ab$abund = as.character(com_table_ex$abund)

  expect_error(table_scarcity(as.data.frame(com_table_ab), "species", "site",
                              "abund"),
               regexp = "Provided abundances are not numeric.")
})
