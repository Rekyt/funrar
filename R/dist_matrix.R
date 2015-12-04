# Function to compute uniqueness on various Databases
#
# Authors: Pierre Denelle & Matthias Grenié
#
# Compute Traits Distance Matrix
# This function computes a Gower's Distance matrix of trait between each pair
# of species present in given 'traits_table', each row represents a species
# and each column a trait.
#
# Arguments:
#   traits_table, a table of traits with species in row and traits in cols
#
# Output:
#   a distance matrix, columns and rows get the same names as 'traits_table'
#   row names.
#
# /!\ WARNING /!\
#   All traits must be present in matrix otherwise computation will crash.
#   Because computed distance is Gower's, traits must be either 'numeric' or
#   'factor' or 'ordered', 'character' will be coerced to 'factor'
#
compute_dist_matrix = function(traits_table) {
  # Use Gower's distance to compute traits distance
  dist_matrix = cluster::daisy(traits_table, metric = "gower") %>%
    as.matrix()

  return(dist_matrix)
}
