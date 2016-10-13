#' Relative abundance matrix from absolute abundance matrix
#'
#' From an abundance matrix (numbers of individuals of a given species at a site)
#' returns a relative abundance matrix (proportion of individuals of a given species
#' at a given site). This function works also with sparse matrices.
#'
#' @param abund_matrix abundance matrix, with sites in rows and species in columns
#'
#' @seealso \code{\link[funrar]{make_absolute}} for the reverse operation
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

#' Abundance matrix from relative abundances matrix
#'
#' Convert the input relative abundances matrix to an abundance matrix
#'
#' @param rel_abund_matrix a relative abundance matrix (proportion of individuals
#'                         of a given species at a given site)
#'
#' @seealso \code{\link[funrar]{make_relative}} for the reverse operation
#'
#' @export
make_absolute = function(rel_abund_matrix) {

  # Compute absolute matrix
  if (requireNamespace("Matrix", quietly = TRUE) &
      is(rel_abund_matrix, "sparseMatrix")) {

    sites_abund = Matrix::rowSums((rel_abund_matrix != 0) &
                                    (!is.na(rel_abund_matrix)), na.rm = TRUE)

    abs_matrix = rel_abund_matrix * sites_abund
  } else {
    # Compute total site abundances
    sites_abund = rowSums((rel_abund_matrix != 0) &
                            (!is.na(rel_abund_matrix)), na.rm = TRUE)

    # Divide each individual abundace by total site abundance
    abs_matrix = sweep(rel_abund_matrix, 1, sites_abund, "*")
  }

  return(abs_matrix)
}
