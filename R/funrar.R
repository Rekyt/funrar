#' Functional Rarity Indices
#'
#' From a site-species matrix and functional distance matrix compute all indices
#' included in the package: functional uniqueness (regional),
#' functional distinctiveness (local), geographical restrictedness (regional),
#' scarcity (local). \strong{Note}: scarcity can only be computed if relative
#' abundances are provided in the site-species matrix.
#'
#' @inheritParams pres_distinctiveness
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
#' @seealso \code{\link[outlieR]{pres_uniqueness}},
#'    \code{\link[outlieR]{pres_distinctiveness}},
#'    \code{\link[outlieR]{pres_restrictedness}},
#'    \code{\link[outlieR]{pres_scarcity}}
#'
#' @export
funrar = function(pres_matrix, dist_matrix, rel_abund = FALSE) {


  funct_uniq = pres_uniqueness(pres_matrix, dist_matrix)

  funct_dist = pres_distinctiveness(pres_matrix, dist_matrix)

  geog_rest = pres_restrictedness(pres_matrix)

  if (rel_abund) {
    scarcity = pres_scarcity(pres_matrix)

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
