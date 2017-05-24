# Functions to compute scarcity on various datasets
#
# Authors: Pierre Denelle & Matthias Grenié
#
#
#' Scarcity for a single community
#'
#' Given a stacked data.frame compute species scarcity. Scarcity measures how
#' abundant is a species locally. Scarcity is close to 1 when a species is rare
#' in a community and close to 0 when it is abundant. See
#' \code{\link[funrar]{scarcity}} function or the functional rarity indices
#' vignette included in the package
#' (type \code{vignette("rarity_indices", package = "funrar")}) for details
#' about the index.
#'
#' @param com_df a stacked (= tidy) data.frame from a single community with
#'    each row representing a species in a community
#'
#' @param sp_col a character vector, the name of the species column in
#'    \code{com_df}
#'
#' @param abund a character vector, the name of the column containing
#'    relative abundances values
#'
#' @return the same data.frame with the additional \strong{Si} column giving
#'    scarcity values for each species
#'
#' @section Caution:
#' This function is meant for internal uses mostly, thus it does not include any
#' tests on inputs and may fail unexpectedly. Please use
#' \code{\link[funrar]{scarcity_stack}} to avoid input errors.
#'
#' @seealso \code{\link[funrar]{scarcity}} and
#' \code{vignette("rarity_indices", package = "funrar")} for details on the
#' scarcity metric;
#' \code{\link[funrar]{distinctiveness_com}} to compute distinctiveness on a
#' single community
#'
#' @export
scarcity_com = function(com_df, sp_col, abund) {

  # Computes scarcity by species
  N_sp = length(com_df[[sp_col]])

  com_df[, "Si"] = exp(-N_sp * log(2) * com_df[[abund]])

  return(com_df)
}

#' Scarcity
#'
#' Compute scarcity values for several communities. Scarcity computation requires
#' relative abundances. Scarcity is close to 1 when a species is rare in a
#' community and close to 0 when it is abundant. See
#' \code{\link[funrar]{scarcity}} function or the functional rarity indices
#' vignette included in the package
#' (type \code{vignette("rarity_indices", package = "funrar")}) for details
#' about the index.
#'
#' @inheritParams scarcity_com
#'
#' @param com a character vector indicating the column name of communities ID in
#'     \code{com_df}
#'
#' @return The same table as \code{com_df} with an added \eqn{S_i} column
#'     for Scarcity values.
#'
#' @seealso \code{\link[funrar]{scarcity}} and
#' \code{vignette("rarity_indices", package = "funrar")} for details on the
#' scarcity metric;
#' \code{\link[funrar]{distinctiveness_stack}},
#' \code{\link[funrar]{restrictedness_stack}},
#' \code{\link[funrar]{uniqueness_stack}}
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe)
#' mat = make_relative(mat)
#' dat = matrix_to_stack(mat, "value", "site", "species")
#' dat$site = as.character(dat$site)
#' dat$species = as.character(dat$species)
#'
#' si_df = scarcity_stack(dat, "species", "site", "value")
#' head(si_df)
#'
#' @export
scarcity_stack = function(com_df, sp_col, com, abund) {

  # Test to be sure of inputs
  full_df_checks(com_df, sp_col, com, abund)
  if (is.null(abund)) {
    stop("No relative abundance provided")
  }

  # Compute Scarcity
  # Split table by communities
  com_split = split(com_df, factor(com_df[[com]]))

  com_split = lapply(com_split,
                      function(one_com)
                        scarcity_com(one_com, sp_col, abund)
  )

  com_scarcity = dplyr::bind_rows(com_split)

  return(com_scarcity)
}

#' Scarcity on site-species matrix
#'
#' Computes scarcity from a relative abundance matrix of species. Scarcity is
#' close to 1 when a species is rare in a community and close to 0 when it is
#' abundant. It requires a site-species matrix with relative abundances. See
#' \code{Details} section for the formula. The sites-species matrix should have
#' \strong{sites} in \strong{rows} and \strong{species} in \strong{columns},
#' similar to \pkg{vegan} package defaults.
#'
#' @param pres_matrix a site-species matrix, with species in rows and sites
#'      in columns, containing \strong{relative abundances} values
#'
#' @return a similar matrix to \code{pres_matrix} with scarcity values in lieu
#'      of relative abundances.
#'
#' @details The scarcity of species is computed as follow:
#'     \deqn{
#'      S_i = \exp{-N \log{2} A_i},
#'     }{%
#'      S_i = exp^(-N*ln2*A_i),
#'      } with \eqn{S_i} the scarcity of species \eqn{i}, \eqn{N} the total
#'     number of species in the community and \eqn{A_i} the relative abundance
#'     of species \eqn{i} in the community. Scarcity is thus a measure of the
#'     \strong{local} rarity in terms of abundances. If \eqn{S_i} is close to 1
#'     the species has a very low abundances while if it's close to 0, it is
#'     quite abundant in the community.
#'
#' @seealso \code{vignette("rarity_indices", package = "funrar")} for details
#' on the scarcity metric;
#' \code{\link[funrar]{distinctiveness}},
#' \code{\link[funrar]{restrictedness}},
#' \code{\link[funrar]{uniqueness}}
#'
#' @examples
#' data("aravo", package = "ade4")
#' # Site-species matrix
#' mat = as.matrix(aravo$spe)
#' mat = make_relative(mat)
#'
#' si = scarcity(pres_matrix = mat)
#' si[1:5, 1:5]
#'
#' @export
scarcity = function(pres_matrix) {

  # Check site-species matrix type
  check_matrix(pres_matrix, "site-species")

  scarcity_mat = pres_matrix

  if (!is_relative(pres_matrix)) {
    warning("Provided object may not contain relative abundances nor ",
            "presence-absence\n",
            "Have a look at the make_relative() function if it is the case")
  }

  # Species with no relative abundance get a scarcity of 0
  scarcity_mat[scarcity_mat == 0] = NA

  # Compute total of nb of species per site (= per row)
  total_sites = apply(scarcity_mat, 1, function(x) sum(x != 0, na.rm = TRUE))

  # Scarcity for each per row using total abundance vector
  scarcity_mat = apply(scarcity_mat, 2, function(x) {
    exp(-total_sites * log(2) * x)
  })

  return(scarcity_mat)
}
