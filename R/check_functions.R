# Internal functions to check inputs and outputs errors
#
# Authors: Pierre Denelle & Matthias Grenié
#

# Stop if provided matrices are not matrices or sparse matrices
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   nothing, shows an error message
check_matrix = function(given_matrix, message = NULL) {

  if (!is.matrix(given_matrix) & !is(given_matrix, "sparseMatrix")) {
    stop(paste0("Provided ", message, " matrix is not a matrix"))
  }
}

# Shows message if distance matrix bigger than site-species matrix
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   nothing, shows a message
check_bigger_dist = function(pres_matrix, dist_matrix) {

  mess = paste("Distance matrix bigger than site-species matrix",
               "Taking subset of distance matrix", sep = "\n")

  if ((ncol(dist_matrix) > ncol(pres_matrix)) |
      (nrow(dist_matrix) > ncol(pres_matrix))) {
    message(mess)
  }

}



# Shows message if site-species matrix is bigger than distance matrix
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   nothing, shows a message
check_bigger_pres = function(pres_matrix, dist_matrix) {
  mess = paste("More species in site-species matrix than in distance matrix\n",
          "Taking subset of site-species matrix", sep = "")

  if ((ncol(pres_matrix) > nrow(dist_matrix)) |
      (ncol(pres_matrix) > ncol(dist_matrix))) {
    message(mess)
  }
}

# Run all the checks and show errors or messages
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   a character vector of names in common
full_matrix_checks = function(pres_matrix, dist_matrix) {
  check_matrix(pres_matrix, "site-species")
  check_matrix(dist_matrix, "distance")
  check_bigger_dist(pres_matrix, dist_matrix)
  check_bigger_pres(pres_matrix, dist_matrix)
}



# Get common species between presence-absence matrix
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   a character vector of names in common
get_common_species = function(pres_matrix, dist_matrix) {

  common_species = intersect(as.character(colnames(pres_matrix)),
                             as.character(colnames(dist_matrix)))

  common_species = intersect(common_species,
                             as.character(rownames(dist_matrix)))

  return(common_species)
}

# Get common species between presence-absence matrix
#
# Args:
#   pres_matrix a site-species matrix
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   a list of species in common if there is any, otherwise shows an error
species_in_common = function(pres_matrix, dist_matrix) {

  common = get_common_species(pres_matrix, dist_matrix)

  if (identical(common, character(0))) {
     stop("No species found in common between matrices")
  } else {
     return(common)
  }
}
