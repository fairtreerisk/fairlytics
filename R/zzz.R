
.onLoad <- function(libname, pkgname) {
  library(conflicted)

  # Explicitly specify which package's function to prefer
  conflict_prefer("calendars", "bizdays")
  conflict_prefer("group_rows", "kableExtra")
  conflict_prefer("last_plot", "ggplot2")
  conflict_prefer("is_empty", "purrr")
  conflict_prefer("is_formula", "purrr")
  conflict_prefer("is_atomic", "purrr")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("renderDataTable", "DT")
}
