# Functions to pass from tidy data.frame to site-species matrix and vice-versa


#' Tidy data.frame to matrix
#'
#' Passes from a tidy data frame (only present species) with an index to a
#' matrix.
#'
#' @param my_df data.frame you want to transform in matrix
#'
#' @param col_to_row character vector of the name of the data.frame column you
#'     want to put into matrix rows
#'
#' @param col_to_col character vector of the name of the data.frame column you
#'     want to be as columns in matrix
#'
#' @param value (optional) character vector indicating the name of a column
#'     coding the values that will be put in the matrix
#'
#' @export
tidy_to_matrix = function(my_df, col_to_row, col_to_col, value = NULL) {

  # Common Formula
  common = paste0("~", col_to_row, "+", col_to_col)

  # Change formula depending on provided column value
  if (is.null(value)) {
    form = common
  } else {
    form = paste0(value, common)
  }


  # Transform to array
  my_mat = xtabs(form, data = my_df, exclude = NULL)

  # Change attributes to get a matrix
  attr(my_mat, "call") = NULL
  class(my_mat) = "matrix"

  return(my_mat)
}


# See with(abund_df, tapply(val, list(species, site), sum))
