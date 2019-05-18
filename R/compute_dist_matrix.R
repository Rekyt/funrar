#' Functional Distance Matrix
#'
#' Wrapper for [cluster::daisy()] function in `cluster` package,
#' to compute distance matrix of trait between each pair of species present in
#' given `traits_table`, each row represents a species and each column
#' a trait. To be able to compute other metrics `traits_table` must have
#' species name as row names.
#'
#' @param traits_table a data.frame of traits with species in row and traits in
#'     columns, **row names** should be **species names**,
#'
#' @param metric character vector in list `'gower'`, `'manhattan'`,
#' `'euclidean'` defining the type of distance to use (see [cluster::daisy()]),
#' see Details section,
#'
#' @param center logical that defines if traits should be centered (only in the
#'               case of `'euclidean'` distance)
#'
#' @param scale logical that defines if traits should be scaled (only in the
#'              case of `'euclidean'` distance)
#'
#'
#' @return
#' A functional distance matrix, **column** and **row** names follow
#' **species name** from `traits_table` row names.
#'
#'
#' @details The functional distance matrix can be computed using any type of
#'     distance metric. When traits are both quantitative and qualitative Gower's
#'     (Gower, 1971; Podani, 1999) distance can be used. Otherwise, any other
#'     distance metric (Euclidean, Manhattan, Minkowski) can be used - as long
#'     as the rows and the columns are named following the species. When using
#'     mixed data consider also Gower's distance extension by Pavoine et al.
#'     (2009). **IMPORTANT NOTE**: in order to get functional rarity indices
#'     between 0 and 1, the distance metric has to be scaled between 0 and 1.
#'
#' @references
#'     Gower, J.C. (1971) A general coefficient of similarity and some of its
#'     properties. Biometrics, 857–871.
#'
#'     Podani, J. (1999) Extending Gower’s general coefficient of similarity
#'     to ordinal characters. Taxon, 331–340.
#'
#'     Pavoine, S., Vallet, J., Dufour, A.-B., Gachet, S., & Daniel, H. (2009)
#'     On the challenge of treating various types of variables: application for
#'     improving the measurement of functional diversity. Oikos, 118, 391–402.
#'
#'
#' @seealso [cluster::daisy()] which this function wraps, base [stats::dist()]
#' or [ade4::dist.ktab()] for Pavoine et al. (2009) extension of Gower's
#' distance.
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
#' @export
compute_dist_matrix = function(traits_table, metric = "gower", center = FALSE,
                               scale = FALSE) {

  if (is.null(rownames(traits_table)) |
      identical(rownames(traits_table), as.character(seq_len(
        nrow(traits_table))))) {
    warning("No row names provided in trait table\n",
            "Distinctiveness and scarcity won't be computable")
  }

  if (metric == "euclidean") {

    if (all(vapply(traits_table, is.numeric, TRUE))) {
      traits_table = scale(traits_table, center, scale)
    } else {
      stop("Non-numeric traits provided. Cannot compute euclidean distance")
    }

  } else if (metric != "euclidean") {

    if (all(vapply(traits_table, is.numeric, TRUE))) {
      warning("Only numeric traits provided, consider using euclidean ",
              "distance.")
    }

    if (center | scale) {
      stop("'", metric, "' distance cannot be scaled nor centered")
    }
  }

  # Use given distance to compute traits distance
  as.matrix(cluster::daisy(traits_table, metric = metric))

}
