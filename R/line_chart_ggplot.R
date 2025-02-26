
#' LineChart
#'
#' @description
#' A function to create an interactive line chart using `ggplot2` and `plotly` with customizable
#' options for the x-axis, y-axis, color, grouping, and chart title.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col A string representing the column name in `df` to be used as the x-axis.
#' @param y_col A string representing the column name in `df` to be used as the y-axis.
#' @param color_col A string representing the column name in `df` to define the color of the lines.
#' @param group_col A string representing the column name in `df` used to group the data into series.
#' @param chart_title A string for the title of the chart.
#'
#' @return A `ggplot2` object representing the interactive line chart.
#'
#' @details
#' This function uses `ggplot2` to create a line chart with the following features:
#' - Data grouped by the `group_col` column for separate line series.
#' - Line colors defined by the `color_col` column.
#' - A chart title and labeled axes.
#' - A continuous y-axis scaled to display percentages.
#' - Conversion of the `ggplot2` object into an interactive `plotly` chart.
#'
#' @examples
#' library(ggplot2)
#' library(plotly)
#' data <- data.frame(
#'   Date = seq.Date(from = Sys.Date() - 10, by = "day", length.out = 10),
#'   Value = runif(10),
#'   ColorGroup = rep(c("Red", "Blue"), each = 5),
#'   Group = rep(c("A", "B"), each = 5)
#' )
#'
#'LineChart(
#'   df = data,
#'   x_col = "Date",
#'   y_col = "Value",
#'   color_col = "ColorGroup",
#'   group_col = "Group",
#'   chart_title = "Interactive Line Chart"
#' )
#'
#'
#' @import ggplot2
#' @import plotly
#' @importFrom scales percent
#' @export
LineChart <- function(df, x_col, y_col, color_col,group_col,chart_title, percent_y_axis = TRUE){

  .inputColumnChecker(df, x_col, y_col, color_col,group_col)

  y_scale_labels <- if (percent_y_axis) scales::percent else waiver()

  p <- ggplot(df, mapping = aes(x = !!sym(x_col),
                                y = !!sym(y_col),
                                color = !!sym(color_col),
                                group = !!sym(group_col))) +
    geom_line() +
    labs(title = chart_title, x = x_col, y = y_col) +
    scale_y_continuous(labels = y_scale_labels) +
    labs(title = chart_title) +
    xlab("")

  return(p)
}

