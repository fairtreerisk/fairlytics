#' Performance Summary Table
#'
#' @description
#' The `PerformanceSummaryTable` function renders a data frame as a styled `kable` object.
#' It uses the `kable` function from the `knitr` package to create a clean and readable table
#' and applies `kable_styling` for additional formatting with a striped style.
#'
#' @param df A data frame representing the performance data to be rendered as a table.
#'
#' @return A `kable` object styled for display in RMarkdown documents or Shiny applications.
#'
#' @export
#'
#' @import kableExtra
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' performance_data <- data.frame(
#'   Metric = c("Month-to-Date", "Year-to-Date", "Inception-to-Date"),
#'   FundA = c("1%", "7%", "15%"),
#'   FundB = c("1.5%", "8%", "12%"),
#'   Difference = c("-0.5%", "-1%", "3%")
#' )
#'
#' PerformanceSummaryTable(performance_data)
PerformanceSummaryTable <- function(df){
  dt <- df %>%
    kable() %>%
    kable_styling(bootstrap_options = 'striped')

  return(dt)
}
