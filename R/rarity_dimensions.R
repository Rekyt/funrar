#' Uniqueness dimensions
#'
#' From a trait table and a site-species matrix compute uniqueness values
#' (nearest functional distance) for each species and each trait, plus computes
#' it for all the traits.
#'
#' @inheritParams restrictedness
#'
#' @param traits_table a stacked (= tidy) data.frame of communities
#'
#' @param ... additional arguments supplied to
#'    \code{\link[funrar]{compute_dist_matrix}}
#'
#' @return A stacked data.frame containing species' names and their uniqueness
#'    values for each traits (\strong{Ui_X} column for trait \strong{X}),
#'    as well as a column for the uniqueness value for all traits
#'    (\strong{Ui_all} column).
#'
#' @seealso \code{\link[funrar]{uniqueness}},
#'    \code{\link[funrar]{uniqueness_stack}} and
#'    \code{\link[funrar]{compute_dist_matrix}} for additional arguments
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#'
#' ui_dim = rarity_dimensions(mat, tra)
#'
#' @export
rarity_dimensions = function(pres_matrix, traits_table, ...) {

  # Other arguments to compute distance matrix
  dots = list(...)

  # Compute distance matrices for each trait
  dist_matrices = lapply(
    seq_along(traits_table),
    function(x, trait = traits_table, other_args = dots) {

      # Call 'compute_dist_matrix()' with supplementary arguments
      do.call("compute_dist_matrix",
              c(list(traits_table = trait[, x, drop = FALSE]), other_args)
      )
    })

  # Rename matrices with trait names
  names(dist_matrices) = colnames(traits_table)

  # Add full distance matrix (all traits)
  dist_matrices[["all"]] = compute_dist_matrix(traits_table, ...)

  # Compute uniqueness data frame for all computed distance matrices
  functional_indices = lapply(
    names(dist_matrices), function(x, matrices = dist_matrices) {
      Ui = uniqueness(pres_matrix, matrices[[x]])

      # Rename Ui column with trait name
      Ui_name = paste0("Ui_", x)
      colnames(Ui)[2] = Ui_name

      return(Ui)
    })

  # Join all data.frames
  Reduce(function(x, y) dplyr::inner_join(x, y, by = "species"),
         functional_indices)
}
