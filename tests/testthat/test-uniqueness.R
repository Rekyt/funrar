# Test Functional Uniqueness Computation
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

# Tests ------------------------------------------------------------------------
test_that("Correct Uniqueness computation", {

  valid_ui = data.frame(species = c("a", "b"), Ui = c(1/9, 1/9))

  all_ui = data.frame(species = letters[1:4],
                      Ui = c(1/9, 1/9, 4/9, 4/9))

  expect_equal(uniqueness_stack(com_df[1:2, ], "species", dist_mat),
               valid_ui)

  expect_error(uniqueness_stack(com_df[1:2, ], "NOT_IN_TABLE", dist_mat),
               regexp = "'NOT_IN_TABLE' column not in provided data.frame")

  expect_message(
    uniqueness_stack(com_df, "species", dist_mat[1:2,]),
    regexp = "^More species in community data.frame than in distance matrix.*"
  )

  expect_equal(uniqueness_stack(com_df, "species", dist_mat), all_ui)

  expect_equal(uniqueness(valid_mat, dist_mat), all_ui)
})
