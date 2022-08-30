#' Other Functional Distinctiveness within range
#'
#' Computes functional distinctiveness from a site-species matrix (containing
#' presence-absence or relative abundances) of species with provided functional
#' distance matrix **considering only species within a given range in the
#' functional space**. The sites-species matrix should have **sites** in
#' **rows** and **species** in **columns**, similar to
#' \pkg{vegan} package defaults.
#'
#' @inheritParams distinctiveness
#'
#' @param given_range a numeric indicating the dissimilarity range at which the
#'                    the other species are considered maximally dissimilar
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
#'    the function using `as(pres_matrix, "dgCMatrix")` from the
#'    `Matrix` package.
#'    (see `vignette("sparse_matrices", package = "funrar")`)
#'
#' @details
#'    The Functional Distinctiveness of a species is the average functional
#'    distance from a species to all the other in the given community. It is
#'    computed as such:
#'    \deqn{
#'    D_i(T) = \frac{
#'                 \sum\limits_{j = 1 ~,j \neq i ~}^S
#'                 \left[ \frac{d_{ij}}{T} + \theta(d_{ij} - T)
#'                   \left(1 - \frac{d_{ij}}{T} \right) \right]
#'               }{
#'                 S - 1
#'               }
#'    }{%
#'    D_i (T) = \Sigma_(j = 0, j != i)^N [d_ij/T  + \theta(d_ij - T)
#'                                        (1 - d_ij/T)] / S - 1
#'
#'    }
#'    with \eqn{D_i} the functional distinctiveness of species \eqn{i}, \eqn{N}
#'    the total number of species in the community and \eqn{d_{ij}}{d_ij} the
#'    functional distance between species \eqn{i} and species \eqn{j}. \eqn{T}
#'    is the chosen maximal range considered. The function
#'    \eqn{\theta(d_ij - T)} is an indicator function that returns 1 when
#'    \eqn{d_{ij} \geq T}{d_ij ≥ T} and 0 when \eqn{d_{ij} < T}{d_ij < T}.
#'    **IMPORTANT NOTE**: in order to get functional rarity indices between 0
#'    and 1, the distance metric has to be scaled between 0 and 1.
#'
#' @importFrom methods is
#'
#' @export
distinctiveness_alt = function(pres_matrix, dist_matrix, given_range) {

  full_matrix_checks(pres_matrix, dist_matrix)

  # Test provided range
  if (!is.numeric(given_range) | is.na(given_range)) {
    stop("'given_range' argument should be non-null and numeric")
  }

  common = species_in_common(pres_matrix, dist_matrix)

  pres_matrix = pres_matrix[, common, drop = FALSE]
  dist_matrix = dist_matrix[common, common]

  if (!is_relative(pres_matrix)) {
    warning("Provided object may not contain relative abundances nor ",
            "presence-absence\n",
            "Have a look at the make_relative() function if it is the case")
  }

  # Correspondance matrix (1 if outside given range, 0 otherwise)
  corr_matrix = dist_matrix
  corr_matrix[dist_matrix >= given_range] = 1
  corr_matrix[dist_matrix < given_range] = 0
  diag(corr_matrix) = 0

  # Matrix product of distance matrix and presence absence matrix
  index_matrix = pres_matrix %*% (dist_matrix / given_range +
                                    (corr_matrix * (1 - dist_matrix /
                                                      given_range)))


  ## Compute sum of relative abundances
  # Replace species not present in communities
  index_matrix[which(pres_matrix == 0)] = NA
  total_sites = rowSums(pres_matrix)


  # Consider denominator (sum of relative abundance or richness - 1)
  denom_matrix = apply(pres_matrix, 2, function(x) total_sites - x)

  index_matrix = index_matrix / denom_matrix
  index_matrix[denom_matrix == 0 & pres_matrix != 0] = 1

  dimnames(index_matrix) = dimnames(pres_matrix)

  return(index_matrix)
}
