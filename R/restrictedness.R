
# Function to compute restrictedness on various databases
#
# Authors: Pierre Denelle & Matthias Grenie
#
#

#' Table Restrictedness
#'
#' Compute restrictedness for every species
#'
#' @param com_table a tidy data.frame of community with column with species
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
#' ri_df = restrictedness(dat, "site", "species")
#' head(ri_df)
#'
#' @export
restrictedness = function(com_table, com, species) {

  # Test to be sure
  if ((is.character(com)) == FALSE) {
    stop("com has to be a character string.")
  }

  if ((is.character(species)) == FALSE) {
    stop("species has to be a character string.")
  }

  if ((com %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any communities.")
  }

  if ((species %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any species.")
  }

  if (!is.character(com_table[[species]])) {
    stop("Provided species are not character.")
  }

  if (!is.character(com_table[[com]])) {
    stop("Provided communities are not character.")
  }

  # get the total number of communities
  n_com = length(unique(com_table[, com]))

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = table(com_table[, species]) / n_com

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
#' @param pres_matrix a presence-absence matrix, with sites in rows and species
#'      in columns
#'
#' @return A tidy data.frame containing species' names and their restrictedness value
#'
#' @examples
#' data("aravo", package = "ade4")
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' ri = pres_restrictedness(mat)
#' head(ri)
#'
#' @export
pres_restrictedness = function(pres_matrix) {

  # Making sure that pres_restrictedness is a matrix
  if (is.matrix(pres_matrix) == FALSE) {
    pres_matrix = as.matrix(pres_matrix)
    warning("Site-species input was not a matrix.")
  }

  # get the total number of communities
  n_com = nrow(pres_matrix)

  # Compute the sum of all species' occurrences divided by n_com
  occupancy = colSums(pres_matrix, na.rm = TRUE) / n_com

  # Format occupancy in data.frame
  occupancy = data.frame("sp" = names(occupancy), "Ri" = as.numeric(occupancy))

  return(occupancy)
}
