
#' Extract Funds
#'
#'#' @description
#' The `ExtractFunds` function processes raw holdings data to extract and format fund information.
#' It creates a data frame with distinct fund names and their descriptions, separating the descriptions
#' into components such as portfolio, benchmark, and differences. Users can choose whether to include
#' benchmark information in the output.
#'
#' @param raw_holdings_data A data frame containing the raw holdings data.
#' It must include the columns specified by `report_col` and `universe_info_col`.
#' @param report_col A string specifying the name of the column that contains fund or report names.
#' @param universe_info_col A string specifying the name of the column containing detailed fund descriptions
#' (e.g., portfolio, benchmark, and differences).
#' @param include_benchmarks A boolean flag indicating whether to include benchmark information in the output.
#' Default is `TRUE`. If set to `FALSE`, the benchmark column will be excluded from the result.
#'
#' @return A data frame with the following columns:
#' - `report_name`: The name of the fund or report.
#' - `port`: The portfolio description extracted from the `header_report_universes`.
#' - `bench`: The benchmark description extracted from the `header_report_universes` (included only if `includeBenchmarks` is `TRUE`).
#' - `diff`: Any additional differences extracted from the `header_report_universes`.
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @importFrom data.table :=
#'
ExtractFunds <- function(raw_holdings_data,report_col, universe_info_col, include_benchmarks = T){

  .inputColumnChecker(raw_holdings_data, report_col, universe_info_col)

  df <- raw_holdings_data %>%
    distinct(!!sym(report_col) := .data[[report_col]],
             !!sym(universe_info_col) := .data[[universe_info_col]]) %>%
    separate(
      !!sym(universe_info_col),
      sep = "\\(1\\) |\\(2\\) |\\(3\\) ",
      into = c("dropme", "port", "bench", "diff"),
      extra = "warn",
      fill = "right"
    ) %>%
    select("report_name" = !!sym(report_col), 'port', 'bench', 'diff')

  if (include_benchmarks == F) {

    df <- df %>% select(-'bench')
  }

  return(df)
}
