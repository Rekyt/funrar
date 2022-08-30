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

  if (!is.matrix(given_matrix) && !is(given_matrix, "Matrix")) {
    stop("Provided ", message, " matrix is not a matrix")
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

  if ((ncol(dist_matrix) > ncol(pres_matrix)) ||
      (nrow(dist_matrix) > ncol(pres_matrix))) {
    message("Distance matrix bigger than site-species matrix\n",
            "Taking subset of distance matrix")
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

  if ((ncol(pres_matrix) > nrow(dist_matrix)) ||
      (ncol(pres_matrix) > ncol(dist_matrix))) {
    message("More species in site-species matrix than in distance matrix\n",
            "Taking subset of site-species matrix")
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



# Test if argument is data.frame shows an error message otherwise
#
# Args:
#   com_df a stacked community data.frame
#
# Returns:
#   nothing, shows an error message if input is not a data.frame
check_df = function(com_df) {
  if (!is.data.frame(com_df)) {
    stop("Provided community data.frame is not a data.frame")
  }
}



# Test if column is in data.frame
#
# Args:
#   com_df a stacked community data.frame
#
#   given_col a character vector for column name
#
# Returns:
#   nothing, shows an error message if column is not in provided data.frame
check_col_in_df = function(com_df, given_col) {
  if (!(given_col %in% colnames(com_df))) {
    stop("'", given_col, "' column not in provided data.frame")
  }
}



# Shows message if more species are present in stacked data.frame than in
# distance matrix
#
# Args:
#   com_df a stacked community data.frame
#
#   sp_col a character vector indicating the column for species
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   nothing, shows a message
check_n_sp_df = function(com_df, sp_col, dist_matrix) {
  df_sp = length(unique(com_df[[sp_col]]))

  mat_sp = min(length(unique(colnames(dist_matrix))),
               length(unique(rownames(dist_matrix))))

  if (df_sp > mat_sp) {
    message("More species in community data.frame than in distance matrix\n",
            "Taking subset of community data.frame")
  } else if (df_sp < mat_sp) {
    message("More species in distance matrix than in community data.frame\n",
            "Taking subset of distance matrix")
  }
}



# Get common species between functional distance matrix and community data.frame
#
# Args:
#   com_df a stacked community data.frame
#
#   sp_col a character vector of the column containing species information
#
#   dist_matrix a functional distance matrix
#
# Returns:
#   a list of species in common if there is any, otherwise shows an error
species_in_common_df = function(com_df, sp_col, dist_matrix) {
  common = intersect(com_df[[sp_col]], colnames(dist_matrix))
  common = intersect(common, rownames(dist_matrix))

  if (identical(common, character(0)) || is.null(common)) {
    stop("No species found in common between distance matrix and data.frame")
  } else {
    return(common)
  }
}



# Run all the checks and show errors if wrong input
#
# Args:
#   com_df a stacked community data.frame
#
#   sp_col a character vector of the column containing species information
#
#   com a character vector of the column containing site information
#
#   abund a character vector of the column containing abundance information
#
#   dist_matrix the functional distance matrix
#
#
# Returns:
#   nothing, shows up errors
full_df_checks = function(com_df, sp_col, com = NULL, abund = NULL,
                          dist_matrix = NULL) {
  check_df(com_df)

  check_col_in_df(com_df, sp_col)

  if (!is.null(com)) {
    check_col_in_df(com_df, com)
  }


  if (!is.null(abund)) {
    check_col_in_df(com_df, abund)

    if (!is.numeric(com_df[[abund]])) {
      stop("Provided abundances are not numeric")
    }
  }

  if (!is.null(dist_matrix)) {
    check_matrix(dist_matrix, "distance")
    check_n_sp_df(com_df, sp_col, dist_matrix)
  }
}
