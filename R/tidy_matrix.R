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
    my_mat = tapply(rep(1, nrow(my_df)), list(my_df[[col_to_row]],
                                                  my_df[[col_to_col]]), sum)
  } else {
    my_mat = tapply(my_df[[col_value]], list(my_df[[col_to_row]],
                                         my_df[[col_to_col]]), sum)
  }

  names(dimnames(my_mat)) = assign_col

  return(my_mat)
}

#' Matrix to tidy data.frame
#'
#' From a matrix with values to a tidy data.frame
#'
#' @param my_mat matrix you want to transform in tidy data.frame
#'
#' @param value_col (optional) character vector to use for value column (default
#' : 'value')
#'
#' @param row_to_col (optional) character vector used for name of column in
#'     data.frame corresponding to rows in matrix (default: corresponding
#'     dimension name)
#'
#' @param col_to_col (optional) character vector used for name of column in
#'     data.frame corresponding to columns in matrix (default: corresponding
#'     dimension name)
#'
#' @export
matrix_to_tidy = function(my_mat, value_col = "value",
                          row_to_col = names(dimnames(my_mat))[1],
                          col_to_col = names(dimnames(my_mat))[2]) {


  if (is.null(row_to_col)) {
    row_to_col = "row"
  }

  if (is.null(col_to_col)) {
    col_to_col = "col"
  }

  tidy_df = as.data.frame(as.table(my_mat))

  tidy_df = na.exclude(tidy_df)

  colnames(tidy_df) = c(row_to_col, col_to_col, value_col)


  tidy_df = tidy_df[, c(col_to_col, row_to_col, value_col)]

  row.names(tidy_df) = NULL

  return(tidy_df)
}
