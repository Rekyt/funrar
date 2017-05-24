#' Relative abundance matrix from absolute abundance matrix
#'
#' From an abundance matrix (numbers of individuals of a given species at a site)
#' returns a relative abundance matrix (proportion of individuals of a given species
#' at a given site). This function works also with sparse matrices.
#'
#' @param abund_matrix abundance matrix, with sites in rows and species in
#'                     columns.
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' head(mat)[, 1:5]  # Has absolute abundances
#' rel_mat = make_relative(mat)
#' head(rel_mat)  # Relative abundances
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
#' @examples
#' data("aravo", package = "ade4")
#'
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' head(mat)[, 1:5]  # Has absolute abundances
#' rel_mat = make_relative(mat)
#' head(rel_mat)  # Relative abundances
#' is_relative(mat)      # FALSE
#' is_relative(rel_mat)  # TRUE
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
