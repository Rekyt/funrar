
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
    stop("Provided distance matrix doesn't have same number of rows and columns.")
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

#' Distinctiveness
#'
#' Compute distinctiveness for several communities.
#'
#' @inheritParams uniqueness
#'
#' @param com a character vector, indicating the column name of communities
#'     names.
#'
#' @param abund optional character vector indicating the column name of the
#'     relative abundances of species, if provided, weight distinctiveness by
#'     relative abundances.
#'
#' @return a table similar to \code{com_table} with an added column \eqn{D_i}
#' @export
distinctiveness = function(com_table, sp_col, com, abund = NULL, dist_matrix) {

  # Test to be sure
  if ((com %in% colnames(com_table)) == FALSE) {
    stop("Community table does not have any communities.")
  }

  if ((sp_col %in% colnames(com_table)) == FALSE) {
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
    stop("Provided distance matrix doesn't have same number of rows and columns.")
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

#' Distinctiveness on presence/absence matrix
#'
#' Computes distinctiveness from a presence-absence matrix of species with a
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
#' @return a similar matrix to presence-absence with distinctiveness at sites?
#'
#' @importFrom dplyr %>% bind_rows
#'
#' @export
pres_distinctiveness = function(pres_matrix, dist_matrix) {

  if (nrow(dist_matrix) != nrow(pres_matrix)) {
    message("Distance matrix > number of species\nTaking subset of distance matrix")
    dist_matrix = dist_matrix[rownames(pres_matrix), rownames(pres_matrix)]
  }

  # Matrix product of distance matrix and presence absence matrix
  index_matrix = dist_matrix %*% pres_matrix

  # Replace species not present in communities
  index_matrix[which(pres_matrix == 0)] = NA

  # Compute sum of relative abundances
  denom_matrix = colSums(pres_matrix) %>%
    # Make a matrix with sum per column of relative abundances in each cell
    rep(nrow(pres_matrix)) %>%
    # Remove focal species
    matrix(byrow = TRUE, ncol = ncol(pres_matrix)) - pres_matrix

  index_matrix = index_matrix / denom_matrix

  if (length(sum(is.nan(index_matrix)))) {
    warning("Some communities had a single species in them\nComputed value assigned to 'NaN'")
  }


  return(index_matrix)
}
