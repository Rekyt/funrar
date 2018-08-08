#' Functional Distinctiveness within range
#'
#' Computes functional distinctiveness from a site-species matrix (containing
#' presence-absence or relative abundances) of species with provided functional
#' distance matrix. The sites-species matrix should have **sites** in
#' **rows** and **species** in **columns**, similar to
#' \pkg{vegan} package defaults.
#'
#' @inheritParams distinctiveness
#'
#' @param given_range a numeric indicating the dissimilarity range at which the
#'                    the influence of other species is not considered anymore
#'
#' @return a similar matrix from provided `pres_matrix` with Distinctiveness
#'    values in lieu of presences or relative abundances, species absent from
#'    communities will have an `NA` value (see `Note` section)
#'
#' @section Note:
#'    Absent species should be coded by `0` or `NA` in input matrices.
#'
#'    When a species is alone in its community the functional distinctiveness
#'    cannot be computed (denominator = 0 in formula), and its value is assigned
#'    as `NaN`.
#'
#'    For speed and memory efficiency sparse matrices can be used as input of
#'    the function using `as(pres_matrix, "sparseMatrix")` from the
#'    `Matrix` package.
#'    (see `vignette("sparse_matrices", package = "funrar")`)
#'
#' @details
#'    The Functional Distinctiveness of a species is the average functional
#'    distance from a species to all the other in the given community. It is
#'    computed as such:
#'    \deqn{
#'    D_i = \frac{\sum_{j = 0, i \neq j}^N d_{ij}}{N-1},
#'    }{%
#'    D_i = ( \Sigma_(j = 0, i != j)^N d_ij) / (N - 1),
#'    }
#'    with \eqn{D_i} the functional distinctiveness of species \eqn{i}, \eqn{N}
#'    the total number of species in the community and \eqn{d_{ij}}{d_ij} the
#'    functional distance between species \eqn{i} and species \eqn{j}.
#'    **IMPORTANT NOTE**: in order to get functional rarity indices between 0
#'    and 1, the distance metric has to be scaled between 0 and 1.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom methods is
#'
#' @examples
#' data("aravo", package = "ade4")
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#'
#' # Compute relative abundances
#' mat = make_relative(mat)
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#' # Distance matrix
#' dist_mat = compute_dist_matrix(tra)
#'
#' di = distinctiveness(pres_matrix = mat, dist_matrix = dist_mat)
#' di[1:5, 1:5]
#'
#' # Compute distinctiveness for all species in the regional pool
#' # i.e., with all the species in all the communities
#' # Here considering each species present evenly in the regional pool
#' reg_pool = matrix(1, ncol = ncol(mat))
#' colnames(reg_pool) = colnames(mat)
#' row.names(reg_pool) = c("Regional_pool")
#'
#' reg_di = distinctiveness(reg_pool, dist_mat)
#'
#' @export
distinctiveness_range = function(pres_matrix, dist_matrix, given_range, relative = FALSE) {

  full_matrix_checks(pres_matrix, dist_matrix)

  # Test provided range
  if (!is.numeric(given_range) | is.na(given_range)) {
    stop("'given_range' argument should be non-null and numeric")
  }

  # Test relative argument
  if (!is.logical(relative) | is.na(relative) | length(relative) != 1) {
    stop("'relative' argument should be either TRUE or FALSE")
  }

  common = species_in_common(pres_matrix, dist_matrix)

  pres_matrix = pres_matrix[, common, drop = FALSE]
  dist_matrix = dist_matrix[common, common]

  if (!is_relative(pres_matrix)) {
    warning("Provided object may not contain relative abundances nor ",
            "presence-absence\n",
            "Have a look at the make_relative() function if it is the case")
  }

  # Correspondance matrix
  corr_matrix = dist_matrix
  corr_matrix[dist_matrix > given_range] = 0
  corr_matrix[dist_matrix <= given_range] = 1
  diag(corr_matrix) = 0

  # Matrix product of distance matrix and presence absence matrix
  index_matrix = pres_matrix %*% (dist_matrix * corr_matrix)


  # Compute sum of relative abundances
  if (requireNamespace("Matrix", quietly = TRUE) &
      is(pres_matrix, "sparseMatrix")) {
    # Replace species not present in communities
    index_matrix[Matrix::which(pres_matrix == 0)] = NA
    total_sites = Matrix::rowSums(pres_matrix)

  } else {

    # Replace species not present in communities
    index_matrix[which(pres_matrix == 0)] = NA
    total_sites = rowSums(pres_matrix)
  }

  # Count the number of species considered for each species
  denom_matrix = pres_matrix %*% corr_matrix

  # Define maximum functional distance per site to standardize
  max_dist = 1
  if (relative) {
    max_dist = apply(pres_matrix, 1, function(row, d_mat = dist_matrix) {
      non_null_sp = names(row[row != 0])  # Select present species in community

      # Community distance matrix
      non_null_dist = d_mat[non_null_sp, non_null_sp]

      # Maximum functional distance
      max(non_null_dist)
    })
  }

  # Define right term in the equation of abundance weighted range Di
  right_term = 1
  if (is_relative(pres_matrix) & !all(unique(as.vector(pres_matrix)) %in%
                                      c(1, 0))) {
    right_term = 1 - denom_matrix
  }

  index_matrix = ((index_matrix / denom_matrix) * right_term) / max_dist
  index_matrix[denom_matrix == 0 & pres_matrix != 0] = 1

  dimnames(index_matrix) = dimnames(pres_matrix)

  return(index_matrix)
}
