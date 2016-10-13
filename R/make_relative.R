#' Relative abundance matrix from absolute abundance matrix
#'
#' From an abundance matrix (numbers of individuals of a given species at a site)
#' returns a relative abundance matrix (proportion of individuals of a given species
#' at a given site). This function works also with sparse matrices.
#'
#' @param abund_matrix abundance matrix, with sites in rows and species in columns
#'
#' @export
make_relative = function(abund_matrix) {

  # Check input type using internal function
  check_matrix(abund_matrix, "abundance")

  # Compute relative abundances matrix
  if (requireNamespace("Matrix", quietly = TRUE) &
      is(abund_matrix, "sparseMatrix")) {

    sites_abund = Matrix::rowSums(abund_matrix, na.rm = TRUE)

    rel_abund_matrix = abund_matrix / sites_abund
  } else {
    # Compute total site abundances
    sites_abund = rowSums(abund_matrix, na.rm = TRUE)

    # Divide each individual abundace by total site abundance
    rel_abund_matrix = sweep(abund_matrix, 1, sites_abund, "/")
  }

  return(rel_abund_matrix)
}
