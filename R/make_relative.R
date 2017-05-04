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

#' Tell if matrix or data.frame has relative abundances
#'
#' From an abundance/presence-absence matrix or data.frame tells if it contains
#' relative abundances or absolute abundances. Checks if all abundances are
#' between 1 and 0 but \strong{never checks sum of abundances per community}.
#'
#' @param given_obj abundance or presence-absence matrix, with sites in rows and
#'                  species in columns, or tidy community data frame
#'
#' @param abund name of the column of the provided object that contains the
#'              abundances
#'
#' @seealso \code{\link[funrar]{make_relative}} to transform matrix into a
#'   relative abundance matrix.
#'
#' @importFrom stats na.omit
is_relative = function(given_obj, abund = NULL) {

  is_rel = FALSE

  if (is.matrix(given_obj) | is(given_obj, "sparseMatrix")) {
    values = na.omit(unique(as.vector(given_obj)))
  } else if (is.data.frame(given_obj) & !is.null(abund) & is.character(abund)) {
    values = na.omit(unique(given_obj[[abund]]))
  }

  max_val = max(values)
  min_val = min(values)

  if (max_val <= 1 & min_val >= 0) {
    is_rel = TRUE
  }

  return(is_rel)
}
