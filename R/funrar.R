# Functions to compute functional rarity indices on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#
#' Functional Rarity Indices
#'
#' From a site-species matrix and functional distance matrix compute all indices
#' included in the package: functional uniqueness (regional, functional),
#' functional distinctiveness (local, functional), geographical restrictedness
#' (regional, extent), scarcity (local, abundance). \strong{Note}: scarcity can
#' only be computed if relative abundances are provided in the site-species
#' matrix.
#'
#' @inheritParams distinctiveness
#'
#' @param rel_abund logical (\code{TRUE} or \code{FALSE}) indicating if
#'    site-species matrix contain relative abundances values or only
#'    presence-absence data (default = \code{FALSE})
#'
#' @return A list of 3 objects (or 4 if \code{rel_abund = TRUE}):
#'    \describe{
#'      \item{\strong{Ui}}{a vector containing uniqueness values per species,}
#'      \item{\strong{Di}}{a site-species matrix with functional distinctiveness
#'        values per species per site,}
#'      \item{\strong{Ri}}{a vector containing geographical restrictedness values
#'        per species,}
#'    }
#'    and if \code{rel_abund = TRUE},
#'    \describe{
#'      \item{\strong{Si}}{a site-species matrix with scarcity values per
#'        species per site.}
#'    }
#'
#' @seealso \code{\link[funrar]{uniqueness}},
#'    \code{\link[funrar]{distinctiveness}},
#'    \code{\link[funrar]{restrictedness}},
#'    \code{\link[funrar]{scarcity}}
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

#' Functional Rarity indices on stacked data.frame
#'
#' From a stacked (= tidy) data.frame and functional distance matrix compute
#' all indices included in the package: functional uniqueness (regional,
#' functional), functional distinctiveness (local, functional), geographical
#' restrictedness (regional, extent), scarcity (local, abundance).
#' \strong{Note}: scarcity can only be computed if relative abundances are
#' provided in the data.frame.
#'
#' @inheritParams distinctiveness_stack
#'
#' @return A list of 3 objects (or 4 if \code{abund} is not \code{NULL}):
#'    \describe{
#'      \item{\strong{Ui}}{a vector containing uniqueness values per species,}
#'      \item{\strong{Di}}{a site-species matrix with functional distinctiveness
#'        values per species per site,}
#'      \item{\strong{Ri}}{a vector containing geographical restrictedness values
#'        per species,}
#'    }
#'    and if \code{abund} is not \code{NULL},
#'    \describe{
#'      \item{\strong{Si}}{a site-species matrix with scarcity values per
#'        species per site.}
#'    }
#'
#' @seealso \code{\link[funrar]{uniqueness_stack}},
#'    \code{\link[funrar]{distinctiveness_stack}},
#'    \code{\link[funrar]{restrictedness_stack}},
#'    \code{\link[funrar]{scarcity_stack}}
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
