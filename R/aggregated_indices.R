#' Function to obtain aggregated (mean and variance) distinctiveness or
#' scarcity per species and community
#'
#' Arguments:
#'
#' com, a character vector combining all communities, each community is repeated
#' by the number of species it contains
#'
#' @param species a character vector counting all the species in each community
#'
#' @param com a character vector giving a list of the communities of all
#' combinations (see ind)
#'
#' @param ind a distinctiveness or scarcity vector of each species/community
#' association, ind is obtained thanks to distinctiveness or scarcity function
#'
#' @return Four vector with attributed names:
#' mean_ind_com contains the index' mean per community
#' mean_ind_sp contains the index' mean per species
#' var_ind_com contains the index' variance per community
#' var_ind_sp contains the index' variance per species
#' skew_ind_com contains the index' skewness per community
#' skew_ind_sp contains the index' skewness per species
#'
#' @importFrom moments skewness
#' @export

agg_ind = function(species, com, ind) {

   if (length(species) != length(ind)) {
     stop("Species/community association and distinctiveness don't have
     same size.")
   }

   if (length(com) != length(ind)) {
     stop("Species/community association and distinctiveness don't have
     same size.")
   }

   if (is.numeric(ind) == FALSE) {
     stop("Provided distinctiveness or scarcity is not numeric.")
   }

   dat = data.frame(com, species, ind)
   colnames(dat) = c("com", "species", "ind")

   mean_ind_sp = sapply(split(dat$ind, dat$sp), mean)
   mean_ind_com = sapply(split(dat$ind, dat$com), mean)
   var_ind_sp = sapply(split(dat$ind, dat$sp), var)
   var_ind_com = sapply(split(dat$ind, dat$com), var)
   skew_ind_sp = sapply(split(dat$ind, dat$sp), skewness)
   skew_ind_com = sapply(split(dat$ind, dat$com), skewness)

   return(list(mean_ind_sp = mean_ind_sp, mean_ind_com = mean_ind_com,
    var_ind_sp = var_ind_sp, var_ind_com = var_ind_com,
    skew_ind_sp = skew_ind_sp, skew_ind_com = skew_ind_com))

}
