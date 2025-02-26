
#' LineChartWrapper
#'
#' @description
#' A wrapper function to create an interactive line chart using either `ggplot2` or `highcharter`,
#' with customizable options for the x-axis, y-axis, color, grouping, and chart title.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col A string representing the column name in `df` to be used as the x-axis.
#' @param y_col A string representing the column name in `df` to be used as the y-axis.
#' @param color_col A string representing the column name in `df` to define the color of the lines.
#' @param group_col A string representing the column name in `df` used to group the data into series.
#' @param chart_title A string for the title of the chart.
#' @param plot_engine A string indicating the plotting engine to use. Can be either `"ggplot2"` or `"highcharter"`.
#'    Default is `"ggplot2"`.
#'
#' @return A `plotly` object (if `"ggplot2"` is chosen) or a `highchart` object (if `"highcharter"` is chosen)
#'         representing the interactive line chart.
#'
#' @details
#' This function creates an interactive line chart with the following features:
#' - Data grouped by the `group_col` column for separate line series.
#' - Line colors defined by the `color_col` column.
#' - A chart title and labeled axes.
#' - A continuous y-axis scaled to display percentages.
#' - Converts the `ggplot2` object into an interactive `plotly` chart or creates a `highcharter` line chart.
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
#' chart_ggplot2 <- LineChartWrapper(
#'   df = data,
#'   x_col = "Date",
#'   y_col = "Value",
#'   color_col = "ColorGroup",
#'   group_col = "Group",
#'   chart_title = "Interactive Line Chart (ggplot2)",
#'   plot_engine = "ggplot2"
#' )
#'
#'chart_ggplot2
#'
#' chart_highcharter <- LineChartWrapper(
#'   df = data,
#'   x_col = "Date",
#'   y_col = "Value",
#'   color_col = "ColorGroup",
#'   group_col = "Group",
#'   chart_title = "Interactive Line Chart (Highcharter)",
#'   plot_engine = "highcharter"
#' )
#' chart_highcharter
#'
#' @import ggplot2
#' @import highcharter
#' @importFrom scales percent
#' @export
LineChartWrapper <- function(df,
                             x_col,
                             y_col,
                             color_col,
                             group_col,
                             chart_title,
                             plot_engine = c("ggplot2", "highcharter"),
                             percent_y_axis = TRUE) {

  .inputColumnChecker(df, x_col, y_col, color_col,group_col)

  p <- switch(plot_engine,
              "ggplot2" = LineChart(df = df,
                                    x_col = x_col,
                                    y_col = y_col,
                                    color_col = color_col,
                                    group_col = group_col,
                                    chart_title = chart_title,
                                    percent_y_axis = TRUE),
              "highcharter" = LineChartr(df = df,
                                         x_col = x_col,
                                         y_col = y_col,
                                         color_col = color_col,
                                         group_col = group_col,
                                         chart_title = chart_title,
                                         percent_y_axis = TRUE)
  )

  return(p)
}



