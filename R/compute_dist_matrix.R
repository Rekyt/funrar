#' Functional Distance Matrix
#'
#' Wrapper for \code{\link[cluster]{daisy}} function in \code{cluster} package,
#' to compute distance matrix of trait between each pair of species present in
#' given \code{traits_table}, each row represents a species and each column
#' a trait. To be able to compute other metrics \code{traits_table} must have
#' species name as row names.
#'
#' @param traits_table a data.sframe of traits with species in row and traits in
#'     columns, \strong{row names} should be \strong{species names},
#'
#' @param metric character vector in list \code{'gower'}, \code{'manhattan'},
#' \code{'euclidean'} defining the type of distance to use (see \code{\link[cluster]{daisy}}),
#' see Details section,
#'
#' @param center logical that defines if traits should be centered (only in the
#'               case of \code{'euclidean'} distance)
#'
#' @param scale logical that defines if traits should be scaled (only in the
#'              case of \code{'euclidean'} distance)
#'
#'
#' @return
#' A functional distance matrix, \strong{column} and \strong{row} names follow
#' \strong{species name} from \code{trait_table} row names.
#'
#'
#' @details The functional distance matrix can be computed using any type of
#'     distance metric. When traits are both quantitative and qualitative Gower's
#'     distance can be used. Otherwise, any other distance metric (Euclidean,
#'     Manhattan, Minkowski) can be used - as long as the rows and the columns
#'     are named following the species.
#'
#' @seealso \code{\link[cluster]{daisy}} which this function wraps,
#'     \code{\link[stats]{dist}}
#'
#' @examples
#' set.seed(1)  # For reproducibility
#' trait = data.frame(
#'    sp = paste("sp", 1:5),
#'    trait_1 = runif(5),
#'    trait_2 = as.factor(c("A", "A", "A", "B", "B")))
#'
#' rownames(trait) = trait$sp
#'
#' dist_mat = compute_dist_matrix(trait[, -1])
#'
#' @aliases distance_matrix
#' @importFrom dplyr %>%
#' @export
compute_dist_matrix = function(traits_table, metric = "gower", center = FALSE,
                               scale = FALSE) {

  if (is.null(rownames(traits_table)) ||
              rownames(traits_table) == as.character(seq_len(
                nrow(traits_table)))) {
    warning(paste("No row names provided in trait table",
                  "Distinctiveness and scarcity won't be computable",
                  sep = "\n"))
  }

  if (metric == "euclidean" & (center | scale)) {
    if (all(vapply(traits_table, is.numeric, TRUE))) {
      traits_table = scale(traits_table, center, scale)
    } else {
      stop("Non-numeric traits provided cannot scale nor center")
    }
  } else if (metric != "euclidean" & (center | scale)) {
    stop("'", metric, "' distance cannot be scaled nor centered")
  }

  # Use given distance to compute traits distance
  dist_matrix = cluster::daisy(traits_table, metric = metric) %>%
    as.matrix()

  return(dist_matrix)
}
