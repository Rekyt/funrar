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
#' @param col_value (optional) character vector indicating the name of a column
#'     coding the values that will be put in the matrix
#'
#' @export
tidy_to_matrix = function(my_df, col_to_row, col_to_col, col_value = NULL) {

  col_names = colnames(my_df)

  assign_col = c(col_to_row, col_to_col)

  if (sum(assign_col %in% col_names) < 2) {

    absent = setdiff(assign_col, col_names)

    if (length(absent) == 1) {

      stop(paste0("Column '", absent, "' is not in data.frame"))

    } else {

      stop(paste0("Columns '", absent[1], "' and '", absent[2], "' are not in data.frame"))

    }

  }

  if (is.null(col_value)) {
    my_mat = tapply(rep(1, nrow(com_table)), list(my_df[[col_to_row]],
                                                  my_df[[col_to_col]]), sum)
  } else {
    my_mat = tapply(my_df[[col_value]], list(my_df[[col_to_row]],
                                         my_df[[col_to_col]]), sum)
  }

  names(dimnames(my_mat)) = assign_col

  return(my_mat)
}
