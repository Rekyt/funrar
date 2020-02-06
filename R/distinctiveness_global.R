#' Global/Regional Functional Distinctiveness from dissimilarity matrix
#'
#' Given a distance (or dissimilarity) matrix or `dist()` objects compute
#' regional/global level ditinctiveness as if all species were present in the
#' same community.
#'
#' @param dist_obj a functional distance matrix as given by
#'    `compute_dist_matrix()`, with species name as row and column names **or**
#'    a `dist()` object with species names as `labels()`
#'
#' @param di_name a character vector giving the name of the distinctiveness
#'    column in the final data.frame (**default**: `global_di`)
#'
#' @return a data.frame with two columns: by default `species` that contains the
#' species names and `global_di` that contains the distinctiveness values.
#' The first column that contains species names can renamed based on `dist_obj`
#' `dimnames`, while the second column is renamed through the `di_name`
#' argument.`
#'
#' @seealso
#' `vignette("rarity_indices", package = "funrar")` and
#' [distinctiveness()] Details section for detail on the index
#'
#' @export
distinctiveness_global = function(dist_obj, di_name = "global_di") {

  # Incorrect input
  if ((!is(dist_obj, "dist") && !is.matrix(dist_obj)) |
      (is.matrix(dist_obj) && ncol(dist_obj) != nrow(dist_obj))) {
    stop("Input should be a dist() object or a square matrix", call. = FALSE)
  }

  if (!is.character(di_name)) {
    stop("Provided column name for regional/global Di should be character",
         call. = FALSE)
  }

  # Always go back to a distance matrix
  if (is(dist_obj, "dist")) {
    dist_obj = as.matrix(dist_obj)
  }

  # Define global pool of species based on distance matrix
  global_pool = matrix(1, nrow = 1, ncol = ncol(dist_obj),
                       dimnames = list(site = "global",
                                       species = colnames(dist_obj)))
  if (!is.null(names(dimnames(dist_obj))[1])) {
    names(dimnames(global_pool))[2] = names(dimnames(dist_obj))[1]
  }

  # Compute distinctiveness
  global_di = distinctiveness(global_pool, dist_obj)

  global_di_df = matrix_to_stack(global_di, di_name, "site",
                                 names(dimnames(global_pool))[2])
  global_di_df = global_di_df[, c(1, 3)]
  global_di_df[, 1] = as.character(global_di_df[, 1])

  return(global_di_df)
}
