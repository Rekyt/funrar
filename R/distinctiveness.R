
# Function to compute distinctiveness on various Databases
#
# Authors: Pierre Denelle & Matthias Grenié
#
#

# Compute distinctiveness for a single community ------------------------------
#
# Arguments: com_table, a tidy data.frame of community with column with species
# and abundance
#
# species, a character vector indicating the name of species column
#
# abund, a character vector in the name of relative abundances column
#
# dist_matrix, the whole distance matrix from region obtained with
# compute_dist_matrix function
#
#
# Output: A tidy data.frame with a new column containing species distinctiveness

single_com_dist = function(com_table, species, abund = NULL, dist_matrix) {

  # Check if distance matrix is a matrix or data frame
  if (!is.matrix(dist_matrix) & !is.data.frame(dist_matrix)) {
    stop("Provided distance matrix should be a 'data.frame' or a 'matrix'")
  }

  # Check if distance matrix has same row and column numbers
  if (nrow(dist_matrix) != ncol(dist_matrix)) {
    stop(paste0("Provided distance matrix doesn't have same number of rows and",
                "columns."))
  }

  # Get functional distance matrix of species in communities
  com_dist <- dist_matrix[com_table[[species]], com_table[[species]]]

  if (!is.null(dim(com_dist))) {
    if (is.null(abund)) {
      # Sum the distances by species
      num <- colSums(com_dist)

      # Number of species minus the focal species
      denom <- nrow(com_table) - 1

    } else {
      # For each species multiplies its functional distance with corresponding
      # abundance to compute distinctiveness
      num <- apply(com_dist, 2, function(x) sum(x * com_table[, abund]))
      # Compute the sum of all abundances minus the one focal species
      denom <- sum(com_table[[abund]]) - com_table[[abund]]
    }
  } else {
    denom = 0
  }

  # Computes distinctiveness by species
  if (denom != 0) {
    com_table[, "Di"] <- as.numeric(num / denom)
  } else {
    com_table[, "Di"] = NA
  }


  return(com_table)
}

#' Table Functional Distinctiveness
#'
#' Compute Functional Distinctiveness for several communities, from a data.frame
#' of communities, with one column for species identity, one for community
#' identity and an optional one for relative abundances.
#'
#' @inheritParams table_uniqueness
#'
#' @param com a character vector, indicating the column name of communities
#'     names.
#'
#' @param abund optional character vector indicating the column name of the
#'     relative abundances of species, if provided, weight distinctiveness by
#'     relative abundances.
#'
#' @return a table similar to \code{com_table} with an added column \eqn{D_i}
#'     giving the Functional Distinctiveness of each species in each communities
#'
#' @export
table_distinctiveness = function(com_table, sp_col, com, abund = NULL,
                                 dist_matrix) {

  # Test to be sure
  if ( (com %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any communities.")
  }

  if ( (sp_col %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any species.")
  }

  if (!is.character(com_table[[sp_col]])) {
    stop("Provided species are not character.")
  }

  if (!is.null(abund) & !is.numeric(com_table[, abund])) {
    stop("Provided abundances are not numeric.")
  }

  if (is.null(abund)) {
    message("No relative abundance provided, computing distinctiveness without
            it.")
  }

  # Check if distance matrix is a matrix or data frame
  if (!is.matrix(dist_matrix) & !is.data.frame(dist_matrix)) {
    stop("Provided distance matrix should be a 'data.frame' or a 'matrix'")
  }

  # Check if distance matrix has same row and column numbers
  if (nrow(dist_matrix) != ncol(dist_matrix)) {
    stop(paste0("Provided distance matrix doesn't have same number of rows and",
                "columns."))
  }

  # Compute Distinctivenness
  # Split table by communities
  com_split <- split(com_table, factor(com_table[[com]]))

  com_split <- lapply(com_split,
                      function(one_com)
                        single_com_dist(one_com, sp_col, abund, dist_matrix)
  )

  com_distinctiveness <- dplyr::bind_rows(com_split)

  return(com_distinctiveness)
}

#' Distinctiveness on presence/absence or relative abundances matrix
#'
#' Computes distinctiveness from a presence-absence matrix (or a matrix with
#' relative abundances) of species with provided functional distance matrix. The
#' sites-species matrix should have \strong{sites} in \strong{rows} and
#' \strong{species} in \strong{columns}, similar to \code{\link[vegan]{vegan}}
#' package defaults.
#'
#' @param pres_matrix a presence-absence matrix (or relative abundances),
#'    with sites in rows and species in columns
#'
#' @param dist_matrix a species functional distance matrix
#'
#' @return a similar matrix from provided \code{pres_matrix}, species absent
#'    from communities will have an \code{NA} value (see \code{Note} section)
#'
#' @section Note:
#'    Absent species should be coded by \code{0} or \code{NA} in input matrices.
#'
#'    When a species is alone in its community the functional distinctiveness
#'    cannot be computed, thus the output matrix contains a value of \code{NaN}
#'    distinctiveness.
#'
#' @details
#'    The Functional Distinctiveness of a species is the average functional
#'    distance from a species to all the other in the given community. It is
#'    computed as such:
#'    \deqn{
#'    D_i = \frac{\sum_{j = 0, i \neq j}^N d_{ij}}{N-1},
#'    }{%
#'    D_i = ( \Sigma_(j = 0, i != j)^N d_ij) / (N - 1),
#'    }
#'    with \eqn{D_i} the functional distinctiveness of species \eqn{i}, \eqn{N}
#'    the total number of species in the community and \eqn{d_{ij}}{d_ij} the
#'    functional distance between species \eqn{i} and species \eqn{j}.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom methods is
#'
#' @export
distinctiveness = function(pres_matrix, dist_matrix) {

  if (nrow(dist_matrix) > ncol(pres_matrix)) {

    message(paste("Distance matrix > Presence Matrix species",
            "Taking subset of distance matrix", sep = "\n"))

    dist_matrix = dist_matrix[colnames(pres_matrix), colnames(pres_matrix)]

  } else if (nrow(dist_matrix) < ncol(pres_matrix)) {

    message(paste("More species in site-species matrix than in provided ",
                  "distance matrix\n", "Taking subset of site-species matrix",
                  sep = ""))

    pres_matrix = pres_matrix[, rownames(dist_matrix)]
  }

  # Matrix product of distance matrix and presence absence matrix
  index_matrix = pres_matrix %*% dist_matrix



  # Compute sum of relative abundances
  if (requireNamespace("Matrix", quietly = TRUE) &
      is(pres_matrix, "sparseMatrix")) {
    # Replace species not present in communities
    index_matrix[Matrix::which(pres_matrix == 0)] = NA
    total_sites = Matrix::rowSums(pres_matrix)

  } else {

    # Replace species not present in communities
    index_matrix[which(pres_matrix == 0)] = NA
    total_sites = rowSums(pres_matrix)
  }

  # Subtract focal species value to site total
  # /!\ need to Transpose because applying function to row tranposes matrix
  denom_matrix = apply(pres_matrix, 2, function(x) total_sites - x)

  index_matrix = index_matrix / denom_matrix

  # Test if there is no NaN in the table for species alone in their community
  if (!all(sapply(index_matrix, function(x) is.numeric(x) | is.na(x)))) {
    warning(paste0("Some communities had a single species in them",
                   "Computed value assigned to 'NaN'", sep = "\n"))
  }


  return(index_matrix)
}
