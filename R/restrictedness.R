# Functions to compute restrictedness on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#

#' Geographical Restrictedness for stacked data.frame
#'
#' Compute the geographical restrictedness for each species present in the
#' stacked data.frame. Geographical restrictedness is an index related to the
#' extent of a species in a given dataset, it is close to 1 when the species is
#' present in only a single site of the dataset (restricted) and close to 0 when
#' the species is present at all sites. It estimates the geographical extent of
#' a species in a dataset. See [restrictedness()] for details
#' on restrictedness computation. You can either use `_stack()` or `_tidy()`
#' functions as they are aliases of one another.
#'
#' @param com_df a stacked (= tidy) data.frame of communities
#'
#' @param sp_col a character vector indicating the name of the species column
#'
#' @param com a character vector indicating the name of the community column
#'
#' @return A stacked data.frame containing species' names and their
#'         restrictedness value in the **Ri** column, similar to what
#'         [uniqueness_stack()] returns.
#'
#' @seealso [restrictedness()], [uniqueness_stack()]
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe)
#' dat = matrix_to_stack(mat, "value", "site", "species")
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

  colnames(occupancy)[1] = sp_col

  occupancy$Ri = 1 - occupancy$Ri

  return(occupancy)
}

# Restrictedness tidy alias
#' @export
#' @rdname restrictedness_stack
restrictedness_tidy = restrictedness_stack



#' Geographical Restrictedness on site-species matrix
#'
#' Computes geographical restrictedness from a site-species matrix.
#' Geographical restrictedness is an index related to the extent of a species
#' in a given dataset, it is close to 1 when the species is present in only a
#' single site of the dataset (restricted) and close to 0 when the species is
#' present at all sites. It estimates the geographical extent of a species in a
#' dataset. See `Details` section to have details on the formula used for
#' the computation. The sites-species matrix should have **sites**
#' in **rows** and **species** in **columns**, similar to \pkg{vegan} package
#' defaults.
#'
#' @param pres_matrix a site-species matrix, with species in rows and sites
#'      in columns, containing presence-absence, relative abundances or
#'      abundances values
#'
#' @return A stacked data.frame containing species' names and their
#'         restrictedness value in the **Ri** column, similar to what
#'         [uniqueness()] returns.
#'
#' @details
#' Geographical Restrictedness aims to measure the regional extent of a species
#' in \pkg{funrar} it is computed the simplest way possible: a ratio of the
#' number of sites where a species is present over the total number of sites in
#' the dataset. We take this ratio off 1 to have a index between 0 and 1 that
#' represents how restricted a species is:
#' \deqn{
#'  R_i = 1 - \frac{N_i}{N_tot},
#' }{
#'  R_i = 1 - (N_i/N_tot),
#' }
#' where \eqn{R_i} is the geographical restrictedness value, \eqn{N_i} the total
#' number of sites where species \eqn{i} occur and \eqn{N_tot} the total number
#' of sites in the dataset.
#' Other approaches can be used to measure the geographical extent (convex hulls,
#' occupancy models, etc.) but for the sake of simplicity only the counting
#' method is implemented in \pkg{funrar}.
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

  if (is.matrix(pres_matrix)) {
    # Convert all the matrix values into 0/1
    pres_matrix[is.na(pres_matrix) == TRUE] = 0
    pres_matrix[pres_matrix > 0] = 1

    # Compute the sum of all species' occurrences divided by n_com
    occupancy = 1 - (colSums(pres_matrix, na.rm = TRUE) / n_com)
  } else if (is(pres_matrix, "sparseMatrix")) {
    # Convert all the matrix values into 0/1
    pres_matrix[Matrix::which(is.na(pres_matrix))] = 0
    pres_matrix[Matrix::which(pres_matrix > 0)] = 1

    occupancy = 1 - (Matrix::colSums(pres_matrix, na.rm = TRUE) / n_com)
  }

  # Format occupancy in data.frame
  occupancy = data.frame("species" = names(occupancy),
                         "Ri" = as.numeric(occupancy))

  return(occupancy)
}
