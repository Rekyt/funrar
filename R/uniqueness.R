#' Uniqueness
#'
#' Computes Uniqueness values over a given regional pool.
#' The uniqueness value of a single species is equal to the
#' minimum functional distance with all other species in the regional pool. The
#' formula is as such:
#' \deqn{U_i = \min(d_{ij}),}
#' with \eqn{d_{ij}} the functional distance between species \eqn{i} and
#' \eqn{j}.
#'
#' @param com_table a data frame of the species in the regional pool.
#'
#' @param sp_col a character vector indicating the name of the species column
#'     in the \code{com_table} data frame
#'
#' @param dist_matrix a functional distance matrix
#'
#'
#' @return a data frame with uniqueness value per species
#'
#'
#' @examples
#' set.seed(1)
#' trait = data.frame(sp = paste0("sp", 1:5), trait_1 = runif(5),
#'     trait_2 = as.factor(c("A", "A", "A", "B", "B")))
#'
#' rownames(trait) = trait$sp
#'
#' dist_mat = compute_dist_matrix(trait[, -1])
#'
#' com_table = data.frame(com = c(rep("com1", 3), rep("com2", 4)),
#'  sp = c("sp1", "sp2", "sp3", "sp2", "sp3", "sp4", "sp5"))
#'
#' com_ui = uniqueness(com_table, "sp", dist_mat)
#'
#'
#' @importFrom dplyr %>%
#' @export
uniqueness = function(com_table, sp_col, dist_matrix) {
  
  if (!(sp_col %in% colnames(com_table))) {
    stop(paste0("'", sp_col, "' species column not in column names"))
  }
  
  if (nrow(dist_matrix) != ncol(dist_matrix)) {
    stop("Distance matrix is not square.")
  }
  
  # Extract all species in community
  com_species = as.character(unique(com_table[[sp_col]]))
  
  # Submatrix containing distance of species in community
  com_dist = dist_matrix[com_species, com_species]
  
  # Replace diagonal by 'NA' for computation reasons
  diag(com_dist) = NA
  
  # Get minimum for each line
  u_index = apply(com_dist, 1, min, na.rm = T)
  
  # Data frame of species name and uniqueness
  u_df = data.frame("sp" = names(u_index), "Ui" = as.numeric(u_index))
  
  return(u_df)
}

#' Uniqueness for presence/Aasence matrix
#'
#' Computes uniqueness from a presence-absence matrix of species with a
#' provided functional distance matrix. The
#' sites-species matrix should have \strong{sites} in \strong{rows} and
#' \strong{species} in \strong{columns}, similar to \code{\link[vegan]{vegan}}
#' package defaults.
#'
#' @inheritParams pres_distinctiveness
#'
#'
#' @return a data frame with uniqueness value per species
#'
#' @export
pres_uniqueness = function(pres_matrix, dist_matrix) {

  com_dist = dist_matrix[colnames(pres_matrix), colnames(pres_matrix)]

  # Replace diagonal by 'NA' for computation reasons
  diag(com_dist) = NA
  
  # Get minimum distance for each species
  u_index = apply(com_dist, 1, min, na.rm = T)

  # Results in a data.frame
  u_df = data.frame("sp" = names(u_index), "Ui" = as.numeric(u_index))
  
  return(u_df)
}
