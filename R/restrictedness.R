
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
#' mat = as.matrix(aravo$spe); dat <- matrix_to_tidy(mat, "value", "site", "species")
#' dat$site = as.character(dat$site)
#' ri_df = table_restrictedness(dat, "site", "species")
#' head(ri_df)
#'
#' @export
table_restrictedness = function(com_df, com, species) {

  # Test to be sure
  if ( (is.character(com)) == FALSE) {
    stop("com has to be a character string.")
  }

  if ( (is.character(species)) == FALSE) {
    stop("species has to be a character string.")
  }

  if ( (com %in% colnames(com_df)) == FALSE) {
    stop("Community table does not have any communities.")
  }

  if ( (species %in% colnames(com_df)) == FALSE) {
    stop("Community table does not have any species.")
  }

  if (!is.character(com_df[[species]])) {
    stop("Provided species are not character.")
  }

  if (!is.character(com_df[[com]])) {
    stop("Provided communities are not character.")
  }

  # get the total number of communities
  n_com = length(unique(com_df[, com]))

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = table(com_df[, species]) / n_com

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

  # Making sure that pres_restrictedness is a matrix
  if (is.matrix(pres_matrix) == FALSE) {
    pres_matrix = as.matrix(pres_matrix)
    warning("Site-species input was not a matrix.")
  }

  # get the total number of communities
  n_com = nrow(pres_matrix)

  # Convert all the matrix values into 0/1
  pres_matrix[is.na(pres_matrix)==T] = 0
  pres_matrix[pres_matrix > 0] = 1

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = colSums(pres_matrix, na.rm = TRUE) / n_com

  # Format occupancy in data.frame
  occupancy = data.frame("sp" = names(occupancy), "Ri" = as.numeric(occupancy))

  return(occupancy)
}
