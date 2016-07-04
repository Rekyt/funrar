
# Function to compute restrictedness on various databases
#
# Authors: Pierre Denelle & Matthias Grenie
#
#

#' Compute restrictedness for every species ------------------------------
#'
#' @param com_table, a tidy data.frame of community with column with species
#'
#' com, a character vector indicating the name of community column
#'
#' species, a character vector indicating the name of species column
#'
#'
#' @return A tidy data.frame containing species' names and their restrictedness value
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
  n_com <- length(unique(com_table$com))
  
  # Compute the sum of all species' occurrences divided by n_com
  occupancy <- table(com_table$species) / n_com
  
  # Format occupancy in data.frame
  occupancy <- data.frame("sp" = names(occupancy), "Ri" = as.numeric(occupancy))
  
  return(occupancy)
}


#' restrictedness on presence/absence matrix
#'
#' Computes restrictedness from a presence-absence matrix of species with a
#' provided functional distance matrix.
#'
#' Experimental for the moment, should be merged with previous function
#' 'distictinctiveness()'
#'
#' @param pres_matrix a presence-absence matrix, with species in rows and sites
#'      in columns (not containing relative abundances for the moments)
#'
#' @inheritParams uniqueness
#'
#' @return a similar matrix to presence-absence with restrictedness at sites?
#'
#' @importFrom dplyr %>% bind_rows
#'
#' @export
pres_restrictedness = function(pres_matrix, dist_matrix) {
  
  if (nrow(dist_matrix) != nrow(pres_matrix)) {
    message("Distance matrix > number of species\nTaking subset of distance matrix")
    dist_matrix = dist_matrix[rownames(pres_matrix), rownames(pres_matrix)]
  }
  
  # Matrix product of distance matrix and presence absence matrix
  index_matrix = dist_matrix %*% pres_matrix
  
  # Replace species not present in communities
  index_matrix[which(pres_matrix == 0)] = NA
  
  # Compute sum of relative abundances
  if (requireNamespace("Matrix", quietly = TRUE) & is(pres_matrix, "sparseMatrix")) {
    total_sites = Matrix::colSums(pres_matrix)
  } else {
    total_sites = colSums(pres_matrix)
  }
  
  # Subtract focal species value to site total
  # /!\ need to Transpose because applying function to row tranposes matrix
  denom_matrix = t(apply(pres_matrix, 1, function(x) total_sites - x))
  
  index_matrix = index_matrix / denom_matrix
  
  if (length(sum(is.nan(index_matrix)))) {
    warning("Some communities had a single species in them\nComputed value assigned to 'NaN'")
  }
  
  
  return(index_matrix)
}
