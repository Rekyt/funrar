# Test Computation of Scarcity

# Valid Presence-Absence Matrix
valid_mat = matrix(c(1, 0, 0, 0,
                     rep(1, 3), 0,
                     0, rep(1, 3),
                     0, 1, 0, 1),
                   ncol = 4)

dimnames(valid_mat) = list("site" = paste0("s", 1:4), "species" = letters[1:4])

# Community Table
log_mat = (valid_mat == 1)

suppressWarnings({
  com_df = lapply(rownames(log_mat), function(x) {
    species = colnames(valid_mat)[log_mat[x, ]]
    data.frame(site = rep(x, length(species)), species = species,
               stringsAsFactors = FALSE)
  })
  com_df = do.call(rbind.data.frame, com_df)
})


# Traits df
trait_df = data.frame(tr1 = c("A", "A", "B", "B"), tr2 = c(rep(0, 3), 1),
                      tr3 = seq(4, 16, 4), stringsAsFactors = TRUE)

rownames(trait_df) = letters[1:4]

# Distance Matrix
dist_mat = compute_dist_matrix(trait_df)


# Distinctiveness data --------------------------------------------------------

# Final distinctiveness table for all communities
correct_dist = data.frame(
  site    = c("s1", "s1", "s2", "s2", "s2", "s3", "s3", "s4", "s4"),
  species = c("a", "b", "b", "c", "d","b", "c", "c", "d"),
  Di      = c(1/9, 1/9, 6/9, 4/9, 6/9, 4/9, 4/9, 4/9, 4/9),
  stringsAsFactors = FALSE
)

correct_dist_mat = table(correct_dist$site, correct_dist$species)

correct_dist_mat[which(correct_dist_mat == 0)] = NA_real_

correct_dist_mat[which(correct_dist_mat == 1)] = correct_dist$Di
correct_dist_mat[2, 3] = 4/9
correct_dist_mat[2, 4] = 6/9

names(dimnames(correct_dist_mat)) = c("site", "species")
# Distinctiveness with abundances
correct_dist_ab = correct_dist


# Undefined Distinctiveness site-species matrix
small_mat = matrix(c(1, 0, 0, 1), nrow = 2)
colnames(small_mat) = letters[1:2]
rownames(small_mat) = c("s1", "s2")

small_df = matrix_to_tidy(small_mat)

# Distinctiveness final data.frame
undef_dist = data.frame(site = c("s1", "s2"), species = c("a", "b"),
                        Di = rep(NaN, 2), stringsAsFactors = FALSE)

# Final distinctiveness matrix
undef_dist_mat = table(undef_dist$site, undef_dist$species)

undef_dist_mat[which(undef_dist_mat == 0)] = NA_real_

undef_dist_mat[which(undef_dist_mat == 1)] = undef_dist$Di

suppressWarnings({
  suppressMessages({
    undef_test = distinctiveness(small_mat, dist_mat)
  })
})


# Scarcity data ----------------------------------------------------------------
com_df_ex = cbind(
  com_df,
  data.frame(abund = c(0.3, 0.7, 0.2, 0.6, 0.2, 0.5, 0.5, 0.2, 0.8))
)
abund_mat = valid_mat
abund_mat[abund_mat == 1] = com_df_ex[order(com_df_ex$species), "abund"]

scarcity_mat = apply(abund_mat, 1, function(x) {
  ifelse(x != 0, exp(-sum(x != 0)*log(2)*x), NA)
})

scarcity_mat = t(scarcity_mat)

com_scarcity = aggregate(species ~ site, data = com_df_ex,
                         function(x) sum(x != 0))

colnames(com_scarcity)[2] = "N_sp"

com_scarcity = merge(com_df_ex, com_scarcity, by = "site")
com_scarcity$Si = exp(-com_scarcity$N_sp*log(2)*com_scarcity$abund)
com_scarcity = com_scarcity[, c(1:3, 5)]
rownames(com_scarcity) = NULL

abund_com = matrix_to_stack(abund_mat, value_col = "abund", row_to_col = "site",
                  col_to_col = "species")
abund_com = subset(abund_com, abund > 0 & site == "s3")
abund_com$Di = c(4/9, 4/9)




# Test for Scarcity ------------------------------------------------------------

test_that("Correct Scarcity computation", {

  # Single community scarcity correct computation
  expect_equal(subset(com_scarcity, site == "s1"),
               scarcity_com(subset(com_df_ex, site == "s1"),
                            "species", "abund"))

  # Scarcity correct computation over many communities
  expect_equal(com_scarcity, scarcity_stack(com_df_ex,
                                            "species", "site", "abund"))

  # Correct Sparseness computation for a site-species matrix
  expect_equal(scarcity_mat, scarcity(abund_mat))
})


test_that("Scarcity errors with bad input", {
  expect_error(scarcity_stack(com_df_ex,
                              "species", "SITE_NOT_IN_TABLE", "abund"),
               regexp = "'SITE_NOT_IN_TABLE' column not in provided data.frame")

  expect_error(
    scarcity_stack(
      com_df_ex, "SPECIES_NOT_IN_TABLE", "site", "abund"),
    regexp = paste0("'SPECIES_NOT_IN_TABLE' column not in ",
                               "provided data.frame"))

  expect_error(scarcity_stack(com_df_ex, "species", "site", NULL),
               regexp = "No relative abundance provided")

  com_df_ab = com_df_ex

  com_df_ab$abund = as.character(com_df_ex$abund)

  expect_error(scarcity_stack(com_df_ab, "species", "site", "abund"),
               regexp = "Provided abundances are not numeric")

  com_df_ab = com_df_ex
  com_df_ab[["abund"]] = as.numeric(com_df_ab[["abund"]]) * 10

  expect_error(
    scarcity_stack(com_df_ab, "species", "site", "abund"),
    "Abundance are not relative, Scarcity can't be computed", fixed = TRUE
  )
})
