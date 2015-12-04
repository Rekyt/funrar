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
  com_dist <- dist_matrix[com_table[, species], com_table[, species]]

  if (is.null(abund)) {
    # Sum the distances by species
    num <- apply(com_dist, 2, function(x) sum(x))

    # Number of species minus the focal species
    denom <- nrow(com_table) - 1

  } else {
    # For each species multiplies its functional distance with corresponding
    # abundance to compute distinctiveness
    num <- apply(com_dist, 2, function(x) sum(x * com_table[, abund]))
    # Compute the sum of all abundances minus the one focal species
    denom <- sum(com_table[, abund]) - com_table[, abund]
  }

  # Computes distinctiveness by species
  com_table[, "Di"] <- as.numeric(num / denom)

  return(com_table)
}

# Distinctiveness for several communities -------------------------------------
#
# Arguments:
#
#  com_table, a data frame a tidy version of species in occurences in com-
#  -munities
#
#  species,  a character vector, indicating the column name of species of
#  'com_table'
#
#  com, a character vector, indicating the column name of communities
#
#  abund, optional character vector indicating the column name of the relative
#  abundances of species, if provided, weight distinctiveness by relative
#  abundances.
#
#  dist_matrix, matrix of all the functional distances between species which
#  can be obtained with compute_dist_matrix()
#
# Output:
#   a value of distinctiveness for each species/community association
#
#
distinctiveness = function(com_table, species, com, abund = NULL, dist_matrix) {

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

  if (!is.null(abund) & !is.numeric(com_table[, abund])) {
    stop("Provided abundances are not numeric.")
  }

  if (is.null(abund)) {
    warning("No relative abundance provided, computing distinctiveness without
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
                        single_com_dist(one_com, species, abund, dist_matrix)
  )

  com_distinctiveness <- dplyr::bind_rows(com_split)

  return(com_distinctiveness)
}
