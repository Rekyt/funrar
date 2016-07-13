
# Function to compute restrictedness on various databases
#
# Authors: Pierre Denelle & Matthias Grenié
#
#

#' Table Restrictedness
#'
#' Compute restrictedness for every species
#'
#' @param com_df a tidy data.frame of community with column with species
#'
#' @param com a character vector indicating the name of community column
#'
#' @param species a character vector indicating the name of species column
#'
#'
#' @return A tidy data.frame containing species' names and their restrictedness
#'    value
#'
#' @examples
#' data("aravo", package = "ade4")
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe); dat = matrix_to_stack(mat, "value", "site", "species")
#' dat$site = as.character(dat$site)
#' dat$species = as.character(dat$species)
#' ri_df = restrictedness_stack(dat, "species", "site")
#' head(ri_df)
#'
#' @export
restrictedness_stack = function(com_df, sp_col, com) {

  # Test to be sure of inputs
  full_df_checks(com_df, sp_col, com)

  # get the total number of communities
  n_com = length(unique(com_df[, com]))

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = table(com_df[, sp_col]) / n_com

  # Format occupancy in data.frame
  occupancy = data.frame("sp" = names(occupancy), "Ri" = as.numeric(occupancy))

  occupancy$Ri = 1 - occupancy$Ri

  return(occupancy)
}


#' Restrictedness on presence/absence matrix
#'
#' Computes restrictedness from a presence-absence matrix of species with a
#' provided functional distance matrix.
#'
#'
#' @inheritParams scarcity
#'
#' @return A tidy data.frame containing species' names and their restrictedness value
#'
#' @examples
#' data("aravo", package = "ade4")
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' ri = restrictedness(mat)
#' head(ri)
#'
#' @export
restrictedness = function(pres_matrix) {

  # Check site-species matrix type
  check_matrix(pres_matrix, "site-species")

  # get the total number of communities
  n_com = nrow(pres_matrix)

  # Convert all the matrix values into 0/1
  pres_matrix[is.na(pres_matrix) == T] = 0
  pres_matrix[pres_matrix > 0] = 1

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = colSums(pres_matrix, na.rm = TRUE) / n_com

  # Format occupancy in data.frame
  occupancy = data.frame("sp" = names(occupancy), "Ri" = as.numeric(occupancy))

  return(occupancy)
}
