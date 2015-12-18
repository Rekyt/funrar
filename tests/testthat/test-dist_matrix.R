context("Compute Functional Distance Matrix")

test_that("Returned object is a matrix", {

  # Object is matrix
  expect_is(compute_dist_matrix(matrix(1:16, nrow = 4)), "matrix")

  # Error with string
  expect_error(compute_dist_matrix("j"))
})


test_that("Names in distance matrix named after rownames", {
  trait = data.frame(sp = paste("sp", 1:5), trait_1 = 1:5,
                     trait_2 = as.factor(c("A", "A", "A", "B", "B")))


  dist_mat = compute_dist_matrix(trait)


  # No rownames
  expect_identical(rownames(dist_mat), as.character(1:5))
  expect_identical(colnames(dist_mat), as.character(1:5))

  # With defined rownames
  rownames(trait) = trait$sp

  dist_mat = compute_dist_matrix(trait)

  expect_identical(rownames(dist_mat), rownames(trait))
  expect_identical(colnames(dist_mat), rownames(trait))
})


test_that("Distance matrix contains good values", {

  trait = data.frame(sp = paste("sp", 1:5), trait_1 = 1:5,
                     trait_2 = as.factor(c("A", "A", "A", "B", "B")))

  t_dist_mat = compute_dist_matrix(trait)

  dist_mat = compute_dist_matrix(matrix(1:16, nrow = 4))

  # Null diagonal
  expect_equivalent(diag(dist_mat), rep(0, 4))
  expect_equivalent(diag(t_dist_mat), rep(0, 5))

  # Expected values
  expect_equivalent(dist_mat,
                    matrix(c(0, 1/3, 2/3, 1,
                             1/3, 0, 1/3, 2/3,
                             2/3, 1/3, 0, 1/3,
                             1, 2/3, 1/3, 0),
                           nrow = 4))
})
