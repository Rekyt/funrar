library("dplyr")
library("Matrix")
context("Measuring Functional Rarity Dimensions")

# Common Objects ---------------------------------------------------------------
given_traits = data.frame(tr1     = 1:3,
                          tr2     = c(1, 1, 0),
                          tr3     = c(0, 0, 1))

trait_names = colnames(given_traits)

rownames(given_traits) = letters[1:3]

pres_mat = matrix(rep(NA, 9), nrow = 3, ncol = 3)
pres_mat[lower.tri(pres_mat, TRUE)] = 1
pres_mat = apply(pres_mat, 2, rev)
pres_mat[3, 3] = 1

dimnames(pres_mat) = list(sites = paste0("s", 1:3), species = letters[1:3])

sparse_mat = as(pres_mat, "sparseMatrix")

# Tests ------------------------------------------------------------------------
test_that("'rarity_dimensions()' outputs good objects", {
  rar_dim = rarity_dimensions(pres_mat, given_traits)

  ## General type check
  expect_type(rar_dim, "list")
  expect_named(rar_dim, c("Ui", "Di"))

  ## On Ui element
  expect_s3_class(rar_dim$Ui, "data.frame")
  expect_named(rar_dim$Ui, c("species", paste0("Ui_", trait_names), "Ui_all"))
  expect_equal(dim(rar_dim$Ui), c(3, 5))

  ## On Di element
  expect_type(rar_dim$Di, "list")
  expect_named(rar_dim$Di, c(paste0("Di_", trait_names), "Di_all"))
  # Check that all elements are matrices
  expect_true(rar_dim$Di %>%
                vapply(class, c("char")) %>%
                `==`("matrix") %>%
                all())
  # Check that all elements have good dimension
  expect_true(rar_dim$Di %>%
                lapply(dim) %>%  # Get dimensions of each element
                # Is it the same as input matrix?
                lapply(function(x) all.equal(x, dim(pres_mat))) %>%
                as.logical() %>%
                all())  # Is it the case for all elements?
  # Check that all elements are well named
  expect_true(rar_dim$Di %>%
                lapply(colnames) %>%
                lapply(function(x) all.equal(x, colnames(pres_mat))) %>%
                as.logical() %>%
                all())
  expect_true(rar_dim$Di %>%
                lapply(row.names) %>%
                lapply(function(x) all.equal(x, row.names(pres_mat))) %>%
                as.logical() %>%
                all())
})

test_that("'rarity_dimensions()' works with sparse matrices", {
  expect_silent(rarity_dimensions(sparse_mat, given_traits))

  sparse_rar_dim = rarity_dimensions(sparse_mat, given_traits)

  ## General type check
  expect_type(sparse_rar_dim, "list")
  expect_named(sparse_rar_dim, c("Ui", "Di"))

  ## On Ui element
  expect_s3_class(sparse_rar_dim$Ui, "data.frame")
  expect_named(sparse_rar_dim$Ui, c("species", paste0("Ui_", trait_names),
                                    "Ui_all"))
  expect_equal(dim(sparse_rar_dim$Ui), c(3, 5))

  ## On Di element
  expect_type(sparse_rar_dim$Di, "list")
  expect_named(sparse_rar_dim$Di, c(paste0("Di_", trait_names), "Di_all"))
  # Check that all elements are matrices
  expect_true(sparse_rar_dim$Di %>%
                vapply(class, c("char")) %>%
                `==`("dgeMatrix") %>%
                all())
  # Check that all elements have good dimension
  expect_true(sparse_rar_dim$Di %>%
                lapply(dim) %>%  # Get dimensions of each element
                # Is it the same as input matrix?
                lapply(function(x) all.equal(x, dim(pres_mat))) %>%
                as.logical() %>%
                all())  # Is it the case for all elements?
  # Check that all elements are well named
  expect_true(sparse_rar_dim$Di %>%
                lapply(colnames) %>%
                lapply(function(x) all.equal(x, colnames(pres_mat))) %>%
                as.logical() %>%
                all())
  expect_true(sparse_rar_dim$Di %>%
                lapply(row.names) %>%
                lapply(function(x) all.equal(x, row.names(pres_mat))) %>%
                as.logical() %>%
                all())
})
