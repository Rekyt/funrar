# Functions to compute functional distinctiveness on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#

#' Functional Distinctiveness for a single community
#'
#' Given a stacked data.frame and a distance matrix compute the functional
#' distinctiveness for a single community. Functional distinctiveness relates to
#' the functional "originality" of a species in a community. The closer to 1 the
#' more the species is functionally distinct from the rest of the community. See
#' [distinctiveness()] function or the functional rarity
#' indices vignette included in the package
#' (type `vignette("rarity_indices", package = "funrar")`), for more details
#' on the metric.
#'
#' @inheritParams scarcity_com
#'
#' @param dist_matrix a functional distance matrix as given by
#'    `compute_dist_matrix()`, with species name as row and column names
#'
#' @return the same data.frame with the additional **Di** column giving
#'    functional distinctiveness values for each species
#'
#' @section Caution:
#' This function is meant for internal uses mostly, thus it does not include any
#' tests on inputs and may fail unexpectedly. Please use
#' [distinctiveness_stack()] to avoid input errors.
#'
#' @seealso [scarcity_com()],
#' `vignette("rarity_indices", package = "funrar")` and
#' [distinctiveness()] Details section for detail on the index
#'
#' @export
distinctiveness_com = function(com_df, sp_col, abund = NULL, dist_matrix) {

  common = species_in_common_df(com_df, sp_col, dist_matrix)

  com_df = subset(com_df, com_df[[sp_col]] %in% common)

  # Get functional distance matrix of species in communities
  com_dist = dist_matrix[common, common]

  if (!is.null(dim(com_dist))) {
    if (is.null(abund)) {
      # Sum the distances by species
      num = colSums(com_dist)

      # Number of species minus the focal species
      denom = nrow(com_df) - 1

    } else {
      # For each species multiplies its functional distance with corresponding
      # abundance to compute distinctiveness
      num = apply(com_dist, 2, function(x) sum(x * com_df[, abund]))
      # Compute the sum of all abundances minus the one focal species
      denom = sum(com_df[[abund]]) - com_df[[abund]]
    }
  } else {
    denom = 0
  }

  # Computes distinctiveness by species
  if (length(denom) > 1) {
    com_df[, "Di"] = as.numeric(num / denom)
  } else if (length(denom) == 1 & denom != 0) {
    com_df[, "Di"] = as.numeric(num / denom)
  } else {
    com_df[, "Di"] = NaN
  }


  return(com_df)
}

#' Functional Distinctiveness on a stacked data.frame
#'
#' Compute Functional Distinctiveness for several communities, from a stacked
#' (or tidy) data.frame of communities, with one column for species identity,
#' one for community identity and an optional one for relative abundances. Also
#' needs a species functional distances matrix. Functional distinctiveness
#' relates to the functional "originality" of a species in a community. The
#' closer to 1 the more the species is functionally distinct from the rest of
#' the community. See [distinctiveness()] function or the
#' functional rarity indices vignette included in the package
#' (type `vignette("rarity_indices", package = "funrar")`), for more details
#' on the metric.
#'
#' @inheritParams distinctiveness_com
#'
#' @param com a character vector, the column name for communities names
#'
#' @return the same data.frame with the additional **Di** column giving
#'    functional distinctiveness values for each species
#'
#' @seealso [scarcity_stack()],
#' [uniqueness_stack()],
#' [restrictedness_stack()];
#' [distinctiveness()] Details section for detail on the index
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#' # Distance matrix
#' dist_mat = compute_dist_matrix(tra)
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe)
#' mat = make_relative(mat)
#' dat = matrix_to_stack(mat, "value", "site", "species")
#' dat$site = as.character(dat$site)
#' dat$species = as.character(dat$species)
#'
#' di_df = distinctiveness_stack(dat, "species", "site", "value", dist_mat)
#' head(di_df)
#'
#' @export
distinctiveness_stack = function(com_df, sp_col, com, abund = NULL,
                                 dist_matrix) {

  # Test to be sure of inputs
  full_df_checks(com_df, sp_col, com, abund, dist_matrix)

  if (is.null(abund)) {
    message("No relative abundance provided, computing Di without it")
  } else if (!is.null(abund) & !is_relative(com_df, abund)) {
    # Test if provided data.frame contains absolute abundances
    warning("Provided object may not contain relative abundances nor ",
            "presence-absence\n",
            "Have a look at the make_relative() function if it is the case")
  }

  # Take subsets of species if needed between distance matrix and community
  common = species_in_common_df(com_df, sp_col, dist_matrix)

  com_df = subset(com_df, com_df[[sp_col]] %in% common)
  dist_matrix = dist_matrix[common, common]

  # Compute Distinctivenness
  # Split table by communities
  com_split = split(com_df, factor(com_df[[com]]))

  com_split = lapply(com_split,
                      function(one_com)
                        distinctiveness_com(one_com, sp_col, abund, dist_matrix)
  )

  com_distinctiveness = dplyr::bind_rows(com_split)

  return(com_distinctiveness)
}

#' Functional Distinctiveness on site-species matrix
#'
#' Computes functional distinctiveness from a site-species matrix (containing
#' presence-absence or relative abundances) of species with provided functional
#' distance matrix. The sites-species matrix should have **sites** in
#' **rows** and **species** in **columns**, similar to
#' \pkg{vegan} package defaults.
#'
#' @param pres_matrix a site-species matrix (presence-absence or relative
#' abundances), with sites in rows and species in columns
#'
#' @param dist_matrix a species functional distance matrix
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
distinctiveness = function(pres_matrix, dist_matrix) {

  full_matrix_checks(pres_matrix, dist_matrix)

  common = species_in_common(pres_matrix, dist_matrix)

  pres_matrix = pres_matrix[, common, drop = FALSE]
  dist_matrix = dist_matrix[common, common]

  if (!is_relative(pres_matrix)) {
    warning("Provided object may not contain relative abundances nor ",
            "presence-absence\n",
            "Have a look at the make_relative() function if it is the case")
  }

  # Matrix product of distance matrix and presence absence matrix
  index_matrix = pres_matrix %*% dist_matrix



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

  # Subtract focal species value to site total
  # /!\ need to Transpose because applying function to row tranposes matrix
  denom_matrix = apply(pres_matrix, 2, function(x) total_sites - x)

  index_matrix = index_matrix / denom_matrix

  # Test if there is NaN in the table for species alone in their community
  if (any(vapply(index_matrix, function(x) is.nan(x), logical(1)))) {
    warning("Some communities had a single species in them\n",
            "Computed value assigned to 'NaN'")
  }

  dimnames(index_matrix) = dimnames(pres_matrix)

  return(index_matrix)
}
