
#' CalendarReturnsTable
#'
#' @description
#' This function visualizes a data table of calendar returns, with each month represented in columns
#' and a Year-To-Date (YTD) column included as the final column. The table is formatted with
#' percentage-style columns for easy comparison and analysis of monthly and YTD returns.
#'
#' @param df A data frame containing monthly return data with columns for each month
#' (typically named according to `month.abb`) and a `YTD` column representing
#' the year-to-date performance.
#'
#' @return
#' A styled `DT` object (a DataTables JavaScript table rendered in HTML) for easy visualization.
#' The table includes fixed headers, center-aligned text, and a highlighted YTD column
#' for better readability and emphasis.
#'
#' @export
#'
#' @import DT
#' @importFrom DT datatable formatPercentage formatStyle
#' @importFrom htmlwidgets JS
#'
#' @examples
#' # Example data frame for returns visualization
#' returns_df <- data.frame(
#'   Year = c(2024, 2023),
#'   Jan = c(0.05, 0.04),
#'   Feb = c(0.02, 0.03),
#'   Mar = c(0.02, 0.03),
#'   Apr = c(-0.01, 0.00),
#'   May = c(-0.2, 0.00),
#'   Jun = c(-0.01, 0.00),
#'   Jul = c(-0.01, 0.00),
#'   Aug = c(-0.01, 0.01),
#'   Sep = c(-0.01, 0.00),
#'   Oct = c(-0.01, 0.00),
#'   Nov = c(-0.01, 0.00),
#'   Dec = c(-0.01, 0.00),
#'   YTD = c(0.06, 0.07)
#' )
#' CalendarReturnsTable(returns_df)
CalendarReturnsTable <- function(df){
  dt <- datatable(data = df,
                  rownames = FALSE,
                  class = 'cell-border',
                  extensions = "FixedHeader",
                  options = list(dom = 'ft',
                                 ordering = F,
                                 columnDefs = list(list(
                                   className = 'dt-center',
                                   targets = 0:13)
                               ),
                               pageLength = nrow(df),
                               fixedHeader = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$('th').css('border-color', '#FFB376');",
                                 "}"
                               ))) %>%
    formatPercentage(columns = c(month.abb, "YTD"), digits = 2) %>%
    formatStyle("YTD",
                backgroundColor = "#F9D5B8",
                fontWeight = "Bold"
    )

  return(dt)
}

