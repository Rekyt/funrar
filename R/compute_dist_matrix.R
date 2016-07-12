#' Functional Distance Matrix
#'
#' Wrapper for \code{"daisy()"} function in \code{"cluster"} package, to compute
#' distance matrix of trait between each pair of species present in given
#' \code{"traits_table"}, each row represents a species and each column a trait.
#' To be able to compute other distances trait table must have species as row
#' names.
#'
#' @param traits_table A data frame of traits with species in row and traits in
#'     columns, row names should be species names,
#'
#' @param metric character vector in list \code{"c('gower', 'manhattan',
#' euclidean')"} defining the type of distance to use
#' (see\code{\link[cluster]{daisy}}).
#'
#' @return A Gower's distance matrix (Gower's 1961), columns and rows get same
#'     names as \code{trait_table} row names. Be sure to give them consistent
#'     row names.
#'
#' @examples
#' set.seed(1)  # For reproducibility
#' trait = data.frame(sp = paste("sp", 1:5), trait_1 = runif(5),
#'    trait_2 = as.factor(c("A", "A", "A", "B", "B")))
#'
#' rownames(trait) = trait$sp
#'
#' dist_mat = compute_dist_matrix(trait[, -1])
#'
#' @aliases distance_matrix
#' @importFrom dplyr %>%
#' @export
compute_dist_matrix = function(traits_table, metric = "gower") {

  if (is.null(rownames(traits_table)) ||
              rownames(traits_table) == as.character(1:nrow(traits_table))) {
    warning(paste("No row names provided in trait table",
                  "Distinctiveness and scarcity won't be computable",
                  sep = "\n"))
  }

  # Use Gower's distance to compute traits distance
  dist_matrix = cluster::daisy(traits_table, metric = metric) %>%
    as.matrix()

  return(dist_matrix)
}
