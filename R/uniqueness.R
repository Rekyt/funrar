# Functions to compute functional uniqueness on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#
#' Functional Uniqueness on stacked data.frame
#'
#' Computes functional uniqueness values over a given regional pool. Functional
#' uniqueness gives the functional distance to the nearest-neighbor of a given
#' species in the provided distance matrix. See [uniqueness()] function for
#' details on computation.
#'
#' @param com_df a data frame of the species in the regional pool.
#'
#' @param sp_col a character vector indicating the name of the species column
#'     in the `com_df` data frame
#'
#' @param dist_matrix a functional distance matrix
#'
#' @return A data.frame with uniqueness value per species, with one column with
#'     provided species column name and the **Ui** column with the uniqueness
#'     values.
#'
#' @seealso
#' [uniqueness()] and `vignette("rarity_indices", package = "funrar")` for
#' details on the uniqueness metric; [distinctiveness_stack()],
#' [restrictedness_stack()], [scarcity_stack()]
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe)
#' dat = matrix_to_stack(mat, "value", "site", "species")
#' dat$site = as.character(dat$site)
#' dat$species = as.character(dat$species)
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#' # Distance matrix
#' dist_mat = compute_dist_matrix(tra)
#'
#' ui_df = uniqueness_stack(dat, "species", dist_mat)
#' head(ui_df)
#'
#' @importFrom dplyr %>%
#' @export
uniqueness_stack = function(com_df, sp_col, dist_matrix) {

  # Test input
  full_df_checks(com_df, sp_col, dist_matrix = dist_matrix)

  # Take subsets of species if needed between distance matrix and community
  common = species_in_common_df(com_df, sp_col, dist_matrix)

  com_dist = dist_matrix[common, common]

  # Replace diagonal by 'NA' for computation reasons
  diag(com_dist) = NA

  # Get minimum for each line
  u_index = apply(com_dist, 1, min, na.rm = TRUE)

  # Data frame of species name and uniqueness
  u_df = data.frame(sp = names(u_index), "Ui" = as.numeric(u_index))

  colnames(u_df)[1] = sp_col

  return(u_df)
}

#' Functional Uniqueness for site-sepcies matrix matrix
#'
#' Computes the functional uniqueness from a site-species matrix with the
#' provided functional distance matrix. Functional Uniqueness represents how
#' "isolated" is a species in the global species pool, it is the functional
#' distance to the nearest neighbor of the species of interest (see `Details`
#' section for the formula). The sites-species matrix should have **sites**
#' in **rows** and **species** in **columns**, similar to \pkg{vegan} package
#' defaults.
#'
#' @inheritParams distinctiveness
#'
#' @details
#' Functional Uniqueness \eqn{U_i} is computed as follow:
#' \deqn{
#'  U_i = \min(d_{ij}) \forall j, j \neq i,
#' }{
#'  U_i = min(d_ij),
#' }
#' with \eqn{U_i} the functional uniqueness of speices \eqn{i}, and \eqn{d_ij}
#' the functional distance between species \eqn{i} and species \eqn{j}
#'
#' @return A data.frame with functional uniqueness values per species, with one
#'     column with provided species column name and the **Ui** column with
#'     functional uniqueness values.
#'
#' @seealso
#' [distinctiveness()], [restrictedness()], [scarcity()]
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' colnames(mat) = as.character(colnames(mat))
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#' # Distance matrix
#' dist_mat = compute_dist_matrix(tra)
#'
#' ui = uniqueness(mat, dist_mat)
#' head(ui)
#'
#' # Computing uniqueness for each community
#' com_ui = apply(mat, 1,
#'                 function(x, dist_m) {
#'                     smaller_com = x[x > 0 & !is.na(x)]
#'                     uniqueness(t(as.matrix(smaller_com)), dist_m)
#'                 }, dist_m = dist_mat)
#'
#' @export
uniqueness = function(pres_matrix, dist_matrix) {

  full_matrix_checks(pres_matrix, dist_matrix)

  common = species_in_common(pres_matrix, dist_matrix)

  pres_matrix = pres_matrix[, common, drop = FALSE]
  dist_matrix = dist_matrix[common, common]

  com_dist = dist_matrix

  # Replace diagonal by 'NA' for computation reasons
  diag(com_dist) = NA

  # Get minimum distance for each species
  u_index = apply(com_dist, 1, min, na.rm = TRUE)

  # Results in a data.frame
  u_df = data.frame("species" = names(u_index), "Ui" = as.numeric(u_index))

  return(u_df)
}
