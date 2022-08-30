#' Uniqueness across combinations of traits
#'
#' From a trait table and a site-species matrix compute Uniqueness
#' (nearest functional distance) for each species and each trait, plus computes
#' it for all the traits.
#'
#' @inheritParams restrictedness
#'
#' @inheritParams combination_trait_dist
#'
#' @return a data.frame containing species' names and their uniqueness values
#'         for each traits (**Ui_X** column for trait **X**), as well as a
#'         column for the uniqueness value for all traits (**Ui_all** column)
#'
#' @seealso [distinctiveness_dimensions()], [uniqueness()], [uniqueness_stack()]
#'          and [compute_dist_matrix()] for additional arguments
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' rel_mat = make_relative(mat)
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#'
#' ui_dim = uniqueness_dimensions(rel_mat, tra)
#'
#' @export
uniqueness_dimensions = function(pres_matrix, traits_table, ...) {

  dist_matrices = combination_trait_dist(traits_table, ...)

  ## Compute Uniqueness
  # Compute uniqueness data frame for all computed distance matrices
  functional_uniqueness = lapply(
    names(dist_matrices), function(x, matrices = dist_matrices) {
      Ui = uniqueness(pres_matrix, matrices[[x]])

      # Rename Ui column with trait name
      Ui_name = paste0("Ui_", x)
      colnames(Ui)[2] = Ui_name

      return(Ui)
    })

  # Join all data.frames for Uniqueness
  Ui = Reduce(function(x, y) merge(x, y, by = "species"),
              functional_uniqueness)

  return(Ui)
}

#' Distinctiveness across combinations of traits
#'
#' From a trait data.frame and a site-species matrix compute Distinctiveness
#' (average pairwise functional distance) for each species in each community
#' on each provided trait and on all traits taken altogether.
#'
#' @inheritParams uniqueness_dimensions
#'
#' @return a list of site-species matrix with functional distinctiveness values
#'         per species per site, with elements **Di_X** for distinctiveness
#'         computed on trait **X** and **Di_all** for distinctiveness computed
#'         on all traits.
#'
#' @seealso [uniqueness_dimensions()], [distinctiveness()],
#'          [distinctiveness_stack()] and [compute_dist_matrix()] for additional
#'          arguments
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' rel_mat = make_relative(mat)
#'
#' # Example of trait table
#' tra = aravo$traits[, c("Height", "SLA", "N_mass")]
#'
#' di_dim = distinctiveness_dimensions(rel_mat, tra)
#'
#' @export
distinctiveness_dimensions = function(pres_matrix, traits_table, ...) {
  dist_matrices = combination_trait_dist(traits_table, ...)

  Di_list = lapply(
    names(dist_matrices), function(x, matrices = dist_matrices) {
      Di = distinctiveness(pres_matrix, matrices[[x]])

      return(Di)
    })

  names(Di_list) = paste0("Di_", names(dist_matrices))

  return(Di_list)
}


#' Compute Multiple distance matrices from a single trait table
#'
#' Internal function to compute combinations of distance matrices from a
#' data.frame of traits, using [compute_dist_matrix()].
#'
#' @inheritParams compute_dist_matrix
#'
#' @param ... additional arguments supplied to [compute_dist_matrix()]
#'
#' @return A list of functional distance matrices, one for each provided trait
#'         plus an additional matrix for all traits taken altogether
combination_trait_dist = function(traits_table, ...) {
  # Other arguments to compute distance matrix
  dots = list(...)

  # Compute distance matrices for each trait
  dist_matrices = lapply(
    seq_along(traits_table),
    function(x, trait = traits_table, other_args = dots) {

      # Call 'compute_dist_matrix()' with supplementary arguments
      do.call(
        "compute_dist_matrix",
        c(list(traits_table = traits_table[, x, drop = FALSE]), other_args)
      )
    })

  # Rename matrices with trait names
  names(dist_matrices) = colnames(traits_table)

  # Add full distance matrix (all traits)
  dist_matrices[["all"]] = compute_dist_matrix(traits_table, ...)

  return(dist_matrices)
}
