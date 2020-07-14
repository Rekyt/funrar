# Test Computation of Restrictedness

# Data -------------------------------------------------------------------------
valid_mat = matrix(c(1, 0, 0, 0,
                     rep(1, 3), 0,
                     0, rep(1, 3),
                     0, 1, 0, 1),
                   ncol = 4)
dimnames(valid_mat) = list("site" = paste0("s", 1:4), "species" = letters[1:4])

log_mat = (valid_mat == 1)

suppressWarnings({
  com_df = lapply(rownames(log_mat), function(x) {
    species = colnames(valid_mat)[log_mat[x, ]]
    data.frame(site = rep(x, length(species)), species = species,
               stringsAsFactors = FALSE)
  })
  com_df = do.call(rbind.data.frame, com_df)
})

# Tests ------------------------------------------------------------------------
test_that("Restrictedness computations work", {
  expect_equal(restrictedness_stack(com_df, "species", "site"),
               data.frame("species" = letters[1:4],
                          "Ri" = c(3/4, 1/4, 1/4, 1/2)))

  expect_equal(restrictedness(valid_mat),
               data.frame("species" = letters[1:4],
                          "Ri" = c(3/4, 1/4, 1/4, 1/2)))

  # With relative restrictedness

  expect_equal(restrictedness_stack(com_df, "species", "site", relative = TRUE),
               data.frame("species" = letters[1:4],
                          "Ri" = c(1, 1/3, 1/3, 2/3)))

  expect_equal(restrictedness(valid_mat, relative = TRUE),
               data.frame("species" = letters[1:4],
                          "Ri" = c(1, 1/3, 1/3, 2/3)))
})

test_that("Restrictedness works with sparse matrices", {
  library(Matrix)
  sparse_mat = as(valid_mat, "sparseMatrix")

  expect_silent(restrictedness(sparse_mat))

  expect_equal(restrictedness(sparse_mat),
               data.frame("species" = letters[1:4],
                          "Ri" = c(3/4, 1/4, 1/4, 1/2)))
})

