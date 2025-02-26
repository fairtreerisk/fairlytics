
#' LineChartr
#'
#' @description
#' A function to create a line chart using the `highcharter` library with customizable options
#' for the x-axis, y-axis, and color grouping. The chart includes exporting functionality
#' and other stylistic customizations.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col A string representing the column name in `df` to be used as the x-axis.
#' @param y_col A string representing the column name in `df` to be used as the y-axis.
#' @param color_col A string representing the column name in `df` to determine the color of the series (optional).
#' @param group_col A string representing the column name in `df` used to group the data into series.
#' @param chart_title A string for the title of the chart.
#'
#' @return A `highchart` object representing the line chart.
#'
#' @details
#' This function creates a line chart with the following features:
#' - Grouping by the `group_col` column for different series.
#' - Optional customization of series colors using `color_col`.
#' - A chart title and labeled axes.
#' - Export functionality to download the chart with a white background.
#' - A disabled range selector for simplicity.
#' - Default styling that disables markers for a cleaner look.
#'
#' @examples
#' library(highcharter)
#' library(lubridate)
#' data <- data.frame(
#'   Date = seq.Date(from = Sys.Date() - 10, by = "day", length.out = 10),
#'   Value = rnorm(10),
#'   Group = rep(c("A", "B"), each = 5)
#' )
#' chart <- LineChartr(
#'   df = data,
#'   x_col = "Date",
#'   y_col = "Value",
#'   color_col = NULL,
#'   group_col = "Group",
#'   chart_title = "Sample Line Chart"
#' )
#' chart
#'
#' @import highcharter
#' @importFrom lubridate today
#' @export
LineChartr <- function(df, x_col, y_col, color_col,group_col, chart_title, percent_y_axis = TRUE){

  .inputColumnChecker(df, x_col, y_col, color_col,group_col)

  p <- hchart(df, "line", hcaes(x = !!sym(x_col), y = !!sym(y_col), group = !!sym(group_col))) %>%
    hc_exporting(enabled = TRUE, filename = paste0(chart_title)) %>%
    hc_rangeSelector(enabled = FALSE) %>%
    hc_title(text = chart_title) %>%
    hc_yAxis(title = list(text = y_col)) %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
    hc_chart(backgroundColor = "#ffffff")

  if (percent_y_axis) {
    p <- p %>% hc_yAxis(title = list(text = y_col), labels = list(format = "{value}%"))
  } else {
    p <- p %>% hc_yAxis(title = list(text = y_col))
  }

  # white background color image download fix
  p$x$theme$chart[["backgroundColor"]] <- "#ffffff"

  return(p)
}
