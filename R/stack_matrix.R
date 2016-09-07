# Functions to pass from tidy data.frame to site-species matrix and vice-versa


#' Stacked (= tidy) data.frame to matrix
#'
#' Passes from a stacked (= tidy) data.frame to a matrix.
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
#' @return a matrix with given \code{col_to_row} column in rows and
#' \code{col_to_col} column in columns. If some cells are not present in the
#' data.frame (e.g. some species not present at some sites), the matrix will have
#' a \code{NA}.
#'
#' @seealso \code{\link[funrar]{matrix_to_stack}} for the reverse operation
#' @aliases tidy_to_matrix
#' @examples
#' example = data.frame("sites" = c(rep("1", 3), rep("2", 2)),
#'  "species" = c("A", "B", "C", "B", "D"),
#'   "abundance" = c(0.33, 0.33, 0.33, 0.4, 0.6))
#'
#' mat = stack_to_matrix(example, "sites", "species", "abundance")
#' mat
#'
#' @export
stack_to_matrix = tidy_to_matrix = function(my_df, col_to_row, col_to_col,
                                            col_value = NULL) {

  col_names = colnames(my_df)

  assign_col = c(col_to_row, col_to_col)

  if (sum(assign_col %in% col_names) < 2) {

    absent = setdiff(assign_col, col_names)

    if (length(absent) == 1) {

      stop(paste0("Column '", absent, "' is not in data.frame"))

    } else {

      stop(paste0("Columns '", absent[1], "' and '", absent[2],
                  "' are not in data.frame"))

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

#' Matrix to stacked (= tidy) data.frame
#'
#' From a matrix with values to a stacked (= tidy) data.frame, exclude NA from
#' given data.frame.
#'
#' @param my_mat matrix you want to transform in stacked (= tidy) data.frame
#'
#' @param value_col (optional) character vector to use for value column
#' (default: 'value')
#'
#' @param row_to_col (optional) character vector used for name of column in
#'     data.frame corresponding to rows in matrix (default: corresponding
#'     dimension name)
#'
#' @param col_to_col (optional) character vector used for name of column in
#'     data.frame corresponding to columns in matrix (default: corresponding
#'     dimension name)
#'
#' @return a stacked (= tidy) data.frame with, a column for row names, one for
#' column names and a third one for the values.
#'
#' @seealso \code{\link[funrar]{stack_to_matrix}} for the reverse operation
#' @aliases matrix_to_tidy
#'
#' @importFrom stats na.exclude
#'
#' @examples
#' data("aravo", package = "ade4")
#'
#' # Site-species matrix converted into data.frame
#' mat = as.matrix(aravo$spe)
#' dat = matrix_to_stack(mat, "value", "site", "species")
#' str(dat)
#'
#' @export
matrix_to_stack = matrix_to_tidy = function(my_mat, value_col = "value",
                          row_to_col = names(dimnames(my_mat))[1],
                          col_to_col = names(dimnames(my_mat))[2]) {


  if (is.null(row_to_col)) {
    row_to_col = "row"
  }

  if (is.null(col_to_col)) {
    col_to_col = "col"
  }
  if (is.matrix(my_mat)) {

    # Tidy data frame from contingency table
    tidy_df = as.data.frame(as.table(my_mat))

  } else if (is(my_mat, "sparseMatrix")) {
    # If input matrix is a sparse matrix need to get the data.frame of non-zero
    # values: summary gives a df with first row coordinate, col coord and value
    summ_df = Matrix::summary(my_mat)

    tidy_df = data.frame(rows_index = rownames(my_mat)[summ_df$i],
                         cols_index = colnames(my_mat)[summ_df$j],
                         val = summ_df$x)
  }

  tidy_df = na.exclude(tidy_df)

  colnames(tidy_df) = c(row_to_col, col_to_col, value_col)


  tidy_df = tidy_df[, c(col_to_col, row_to_col, value_col)]

  row.names(tidy_df) = NULL

  return(tidy_df)
}
