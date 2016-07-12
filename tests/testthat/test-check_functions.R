context("Test Check Functions")


# Preliminary Data -------------------------------------------------------------
pres_mat = matrix(1, nrow = 4, ncol = 3)

colnames(pres_mat) = letters[1:3]

dist_mat = matrix(0, nrow = 3, ncol = 3)
colnames(dist_mat) = letters[1:3]
rownames(dist_mat) = letters[1:3]


# Tests ------------------------------------------------------------------------
test_that("Messages show up with wrong input", {
  library(Matrix)

  expect_silent(check_matrix(pres_mat, dist_mat))
  expect_silent(check_matrix(as(pres_mat, "sparseMatrix"), dist_mat))
  expect_silent(check_matrix(pres_mat, as(dist_mat, "sparseMatrix")))

  expect_silent(check_bigger_dist(pres_mat, dist_mat))
  expect_silent(check_bigger_pres(pres_mat, dist_mat))

  expect_silent(full_matrix_checks(pres_mat, dist_mat))
  expect_silent(full_matrix_checks(as(pres_mat, "sparseMatrix"), dist_mat))
  expect_silent(full_matrix_checks(pres_mat, as(dist_mat, "sparseMatrix")))

  # Provided objects not matrices
  expect_error(
    check_matrix(NULL, dist_mat),
    regexp = "Provided site-species matrix is not a matrix"
  )
  expect_error(
    check_matrix(rep(1, 2), dist_mat),
    regexp = "Provided site-species matrix is not a matrix"
  )
  expect_error(
    check_matrix(pres_mat, NULL),
    regexp = "Provided distance matrix is not a matrix"
  )
  expect_error(
    check_matrix(pres_mat, rep(1, 2)),
    regexp = "Provided distance matrix is not a matrix"
  )

  # Bigger distance matrix
  expect_message(
    check_bigger_dist(pres_mat[,1:2], dist_mat),
    regexp = paste("Distance matrix bigger than site-species matrix",
                   "Taking subset of distance matrix", sep = "\n")
  )

  # Bigger site-species matrix
  expect_message(
    check_bigger_pres(pres_mat, dist_mat[1:2, 1:2]),
    regexp = paste("More species in site-species matrix than in distance matrix\n",
                   "Taking subset of site-species matrix", sep = "")
  )
  expect_message(
    check_bigger_pres(pres_mat, dist_mat[1:2,]),
    regexp = paste("More species in site-species matrix than in distance matrix\n",
                   "Taking subset of site-species matrix", sep = "")
  )
  expect_message(
    check_bigger_pres(pres_mat, dist_mat[, 1:2]),
    regexp = paste("More species in site-species matrix than in distance matrix\n",
                   "Taking subset of site-species matrix", sep = "")
  )

})

test_that("Find Common species between matrices", {
  expect_equal(get_common_species(pres_mat, dist_mat), letters[1:3])

  expect_equal(get_common_species(pres_mat[,1:2], dist_mat), letters[1:2])
  expect_equal(get_common_species(pres_mat, dist_mat[1:2, 1:2]), letters[1:2])
  expect_equal(get_common_species(pres_mat, dist_mat[1:2,]), letters[1:2])
  expect_equal(get_common_species(pres_mat, dist_mat[, 1:2]), letters[1:2])

  expect_identical(get_common_species(pres_mat[0, 0], dist_mat), character(0))
  expect_identical(get_common_species(pres_mat, dist_mat[0, 0]), character(0))
})

test_that("No common species between matrices gives an error", {
  bad_mat = pres_mat
  colnames(bad_mat) = letters[4:6]

  expect_silent(species_in_common(pres_mat, dist_mat))

  expect_equal(species_in_common(pres_mat, dist_mat), letters[1:3])

  expect_error(
    species_in_common(bad_mat, dist_mat),
    regexp = "No species found in common between matrices"
  )
})


