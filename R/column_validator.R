# check for input column names passed by the user.

#' .inputColumnChecker
#'
#' @description
#' Check for Input Column Names Passed by the User
#'
#' @param df A data frame.
#' @param ... Names of the columns to check for.
#' @return Throws an error if columns are not found.
#'
#' @export
.inputColumnChecker <- function(df, ...) {

  col_names <- c(...)
  cols_not_found <- vector()
  df_headers <- colnames(df)

  for (tmp_name in col_names) {
    is_found <- any(grepl(tmp_name, df_headers))
    if (!is_found) {
      cols_not_found <- append(cols_not_found, tmp_name)
    }
  }

  if (length(cols_not_found) > 0)
    stop(paste("Column names not found:", paste(cols_not_found, collapse = ", ")))
}

