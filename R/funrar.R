# Functions to compute functional rarity indices on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#
#' Compute all Functional Rarity Indices from Matrices
#'
#' From a site-species matrix and functional distance matrix compute all indices
#' included in the package: functional uniqueness (regional, functional),
#' functional distinctiveness (local, functional), geographical restrictedness
#' (regional, extent), scarcity (local, abundance). **Note**: scarcity can
#' only be computed if relative abundances are provided in the site-species
#' matrix.
#'
#' @inheritParams distinctiveness
#'
#' @param rel_abund logical (`TRUE` or `FALSE`) indicating if
#'    site-species matrix contain relative abundances values or only
#'    presence-absence data (default = `FALSE`)
#'
#' @return A list of 3 objects (or 4 if `rel_abund = TRUE`):
#'    \describe{
#'      \item{**Ui**}{a vector containing uniqueness values per species,}
#'      \item{**Di**}{a site-species matrix with functional distinctiveness
#'                    values per species per site,}
#'      \item{**Ri**}{a vector containing geographical restrictedness values
#'                    per species,}
#'    }
#'    and if `rel_abund = TRUE`,
#'    \describe{
#'      \item{**Si**}{a site-species matrix with scarcity values per species
#'                    per site.}
#'    }
#'
#' @seealso [uniqueness()], [distinctiveness()], [restrictedness()],
#'          [scarcity()]
#'
#' @export
funrar = function(pres_matrix, dist_matrix, rel_abund = FALSE) {


  funct_uniq = uniqueness(pres_matrix, dist_matrix)

  funct_dist = distinctiveness(pres_matrix, dist_matrix)

  geog_rest = restrictedness(pres_matrix)

  if (rel_abund) {
    scarcity = scarcity(pres_matrix)

    return(list(Ui = funct_uniq,
                Di = funct_dist,
                Ri = geog_rest,
                Si = scarcity))
  } else {
    return(list(Ui = funct_uniq,
                Di = funct_dist,
                Ri = geog_rest))
  }
}

#' Compute all Functional Rarity Indices from stacked data.frames
#'
#' From a stacked (= tidy) data.frame and functional distance matrix compute
#' all indices included in the package: functional uniqueness (regional,
#' functional), functional distinctiveness (local, functional), geographical
#' restrictedness (regional, extent), scarcity (local, abundance).
#' **Note**: scarcity can only be computed if relative abundances are
#' provided in the data.frame.
#'
#' @inheritParams distinctiveness_stack
#'
#' @return A list of 3 objects (or 4 if `abund` is not `NULL`):
#'    \describe{
#'      \item{**Ui**}{a vector containing uniqueness values per species,}
#'      \item{**Di**}{a site-species matrix with functional distinctiveness
#'                    values per species per site,}
#'      \item{**Ri**}{a vector containing geographical restrictedness values
#'                    per species,}
#'    }
#'    and if `abund` is not `NULL`,
#'    \describe{
#'      \item{**Si**}{a site-species matrix with scarcity values per
#'        species per site.}
#'    }
#'
#' @seealso [uniqueness_stack()], [distinctiveness_stack()],
#'          [restrictedness_stack()], [scarcity_stack()]
#'
#' @export
funrar_stack = function(com_df, sp_col, com, abund = NULL,
                        dist_matrix) {

  funct_dist = distinctiveness_stack(com_df, sp_col, com, abund, dist_matrix)
  funct_uniq = uniqueness_stack(com_df, sp_col, dist_matrix)
  geog_rest = restrictedness_stack(com_df, sp_col, com)

  if (is.null(abund)) {
    return(list(
      Ui = funct_uniq,
      Di = funct_dist,
      Ri = geog_rest
    ))
  } else {
    scar = scarcity_stack(com_df, sp_col, com, abund)
    return(list(
      Ui = funct_uniq,
      Di = funct_dist,
      Ri = geog_rest,
      Si = scar
    ))
  }
}
