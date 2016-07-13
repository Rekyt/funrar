#' Uniqueness
#'
#' Computes Uniqueness values over a given regional pool.
#' The uniqueness value of a single species is equal to the
#' minimum functional distance with all other species in the regional pool. The
#' formula is as such:
#' \deqn{U_i = \min(d_{ij}),}{Ui = min(d_ij),}
#' with \eqn{d_{ij}}{d_ij} the functional distance between species
#' \eqn{i} and \eqn{j}.
#'
#' @param com_df a data frame of the species in the regional pool.
#'
#' @param sp_col a character vector indicating the name of the species column
#'     in the \code{com_df} data frame
#'
#' @param dist_matrix a functional distance matrix
#'
#'
#' @return a data frame with uniqueness value per species
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe); dat = matrix_to_stack(mat, "value", "site", "species")
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
  u_index = apply(com_dist, 1, min, na.rm = T)

  # Data frame of species name and uniqueness
  u_df = data.frame(sp = names(u_index), "Ui" = as.numeric(u_index))

  colnames(u_df)[1] = sp_col

  return(u_df)
}

#' Uniqueness for presence/absence matrix
#'
#' Computes uniqueness from a presence-absence matrix of species with a
#' provided functional distance matrix. The
#' sites-species matrix should have \strong{sites} in \strong{rows} and
#' \strong{species} in \strong{columns}, similar to \code{\link[vegan]{vegan}}
#' package defaults.
#'
#' @inheritParams distinctiveness
#'
#'
#' @return a data frame with uniqueness value per species
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
#' @export
uniqueness = function(pres_matrix, dist_matrix) {

  full_matrix_checks(pres_matrix, dist_matrix)

  common = species_in_common(pres_matrix, dist_matrix)

  pres_matrix = pres_matrix[, common]
  dist_matrix = dist_matrix[common, common]

  com_dist = dist_matrix

  # Replace diagonal by 'NA' for computation reasons
  diag(com_dist) = NA

  # Get minimum distance for each species
  u_index = apply(com_dist, 1, min, na.rm = T)

  # Results in a data.frame
  u_df = data.frame("species" = names(u_index), "Ui" = as.numeric(u_index))

  return(u_df)
}
