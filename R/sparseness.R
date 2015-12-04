# Compute sparseness for a single community -----------------------------------
#
# Arguments:
#   com_table, a tidy data.frame of community with column with species, and
#   abundance
#
#   species, a character vector indicating the name of species column
#
#   abund, a character vector in the name of relative abundances column
#
#
# Output:
#   A tidy data.frame with a new column containing species sparseness

single_com_spar = function(com_table, species, abund) {

  # Computes sparseness by species
  rich_sp <- nrow(com_table)
  com_table[, "Si"] <- exp(-rich_sp * log(2) * com_table[, abund])

  return(com_table)
}

#' Sparseness
#'
#' Compute sparness values for several communities. Sparseness is computed per
#' community. Sparseness corresponds to the rareness of a given species in
#' terms of abundances, as such:
#' \deqn{
#'  S_i = \exp(-N\log{2}A_i),
#' }
#' with \eqn{S_i} the sparseness of species \eqn{i}, \eqn{N} the number of
#' species present in the given community and \eqn{A_i} the relative abundance
#' (in %%) of species \eqn{i}.
#'
#' @param com_table a data frame a tidy version of species in occurences in
#'     communities.
#'
#' @param species a character vector indicating the column of species in
#'     \code{com_table}
#'
#' @param com a character vector indicating the column name of communities ID
#'
#' @param abund a character vector indicating the column name of the relative
#'     abundances of species
#'
#' @return The same table as \code{com_table} with an added \eqn{S_i} column
#'     for Sparseness values.
#' @export
sparseness = function(com_table, species, com, abund) {

  # Test to be sure
  if ((com %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any communities.")
  }

  if ((species %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any species.")
  }

  if (!is.character(com_table[, species])) {
    stop("Provided species are not character.")
  }

  if (is.null(abund)) {
    stop("No relative abundance provided.")
  }

  if (!is.numeric(com_table[, abund])) {
    stop("Provided abundances are not numeric.")
  }

  # Compute Sparseness
  # Split table by communities
  com_split <- split(com_table, factor(com_table[[com]]))

  com_split <- lapply(com_split,
                      function(one_com)
                        single_com_spar(one_com, species, abund)
  )

  com_sparseness <- dplyr::bind_rows(com_split)

  return(com_sparseness)
}

