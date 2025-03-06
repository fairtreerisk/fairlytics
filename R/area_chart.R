#' AreaChart
#'
#' @description
#' A function to create an interactive area chart using `ggplot2` and `plotly` with customizable
#' options for the x-axis, y-axis, fill color, grouping, and chart title.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col A string representing the column name in `df` to be used as the x-axis.
#' @param y_col A string representing the column name in `df` to be used as the y-axis.
#' @param fill_col A string representing the column name in `df` to define the fill color of the areas.
#' @param group_col A string representing the column name in `df` used to group the data into series.
#' @param chart_title A string for the title of the chart.
#' @param percent_y_axis A logical value indicating whether the y-axis should be formatted as percentages.
#'
#' @return A `ggplot2` object representing the interactive area chart.
#'
#' @details
#' This function uses `ggplot2` to create an area chart with the following features:
#' - Data grouped by the `group_col` column for separate area series.
#' - Area fill colors defined by the `fill_col` column.
#' - A chart title and labeled axes.
#' - A continuous y-axis scaled to display percentages if `percent_y_axis` is `TRUE`.
#' - Conversion of the `ggplot2` object into an interactive `plotly` chart.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#' test_data <- data.frame(
#'   Date = seq.Date(from = Sys.Date() - 10, by = "day", length.out = 10),
#'   Value = runif(10),
#'   FillGroup = rep(c("Red", "Blue"), each = 5),
#'   Group = rep(c("A", "B"), each = 5)
#' )
#'
#' AreaChart(
#'   df = test_data,
#'   x_col = "Date",
#'   y_col = "Value",
#'   fill_col = "FillGroup",
#'   group_col = "Group",
#'   chart_title = "Interactive Area Chart"
#' )
#'
#' @import ggplot2
#' @import plotly
#' @importFrom scales percent
#' @export
AreaChart <- function(df, x_col, y_col, fill_col, group_col, chart_title, percent_y_axis = TRUE) {
  .inputColumnChecker(df, x_col, y_col, fill_col, group_col)

  y_scale_labels <- if (percent_y_axis) scales::percent else waiver()

  p <- ggplot(df, mapping = aes(x = !!sym(x_col),
                                y = !!sym(y_col),
                                fill = !!sym(fill_col),
                                group = !!sym(group_col))) +
    geom_area(alpha = 0.6) +
    labs(title = chart_title, x = x_col, y = y_col) +
    scale_y_continuous(labels = y_scale_labels) +
    xlab("")

  return(p)
}
