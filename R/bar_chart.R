#' Generate a Bar Chart Using ggplot2 or Highcharter
#'
#' This function creates a bar chart using either the `ggplot2` or `highcharter` package.
#' It allows customization of various chart attributes such as labels, colors, position type,
#' and text display.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x_col A string specifying the column name to be used for the x-axis.
#' @param y_col A string specifying the column name to be used for the y-axis.
#' @param color_col A string specifying the column name to be used for color grouping.
#' @param bar_type A character string specifying how bars should be positioned.
#'        Options are `"stack"` for stacked bars or `"group"` for grouped bars.
#' @param x_label A string specifying the label for the x-axis.
#' @param y_label A string specifying the label for the y-axis.
#' @param chart_title A string specifying the title of the chart.
#' @param show_bar_text A logical indicating whether to display text labels on bars.
#' @param chart_flip A logical indicating whether to flip the chart (horizontal bars).
#' @param plot_engine A character string specifying which plotting library to use.
#'        Options are `"ggplot2"` (default) or `"highcharter"`.
#' @param axis_text_size A numeric value specifying the text size for axis labels.
#' @param num_abv_grph_size A numeric value specifying the font size of numeric labels above bars.
#' @param txt_space_abv_grh A numeric value specifying the space above the bars for text labels.
#'
#' @return A ggplot or highcharter object representing the bar chart.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   category = c("A", "B", "C"),
#'   value = c(10, 20, 15),
#'   group = c("X", "Y", "X")
#' )
#' BarChart(df, x_col = "category", y_col = "value", color_col = "group",
#'          position_type = "stack", plot_engine = "ggplot2")
#' }
BarChart <- function(df,
                     x_col,
                     y_col,
                     color_col,
                     bar_type = c("stack", "group"),
                     x_label="",
                     y_label="",
                     chart_title = NULL,
                     show_bar_text = FALSE,
                     chart_flip = FALSE,
                     plot_engine = c("ggplot2", "highcharter"),
                     axis_text_size = 12,
                     num_abv_grph_size = 4,
                     txt_space_abv_grh = 0.005){

  bar_type <- match.arg(bar_type)  # Validate bar_type argument

  plot_engine <- match.arg(plot_engine)  # Validate plot engine argument

  p <- switch(plot_engine,
              "ggplot2" = BarChart_ggplot(df = df,
                                          x_col = x_col,
                                          y_col = y_col,
                                          color_col = color_col,
                                          bar_type = bar_type,
                                          x_label = x_label,
                                          y_label =  y_label,
                                          chart_title = chart_title,
                                          show_bar_text = show_bar_text,
                                          chart_flip = chart_flip,
                                          axis_text_size = axis_text_size,
                                          num_abv_grph_size = num_abv_grph_size,
                                          txt_space_abv_grh = txt_space_abv_grh),

              "highcharter" = BarCharter(df = df,
                                         x_col = x_col,
                                         y_col = y_col,
                                         color_col = color_col,
                                         bar_type = bar_type,
                                         x_label = x_label,
                                         y_label =  y_label,
                                         chart_title = chart_title,
                                         show_bar_text = show_bar_text,
                                         chart_flip = chart_flip,
                                         axis_text_size = axis_text_size,
                                         num_abv_grph_size = num_abv_grph_size,
                                         txt_space_abv_grh = txt_space_abv_grh)
  )

  return(p)
}

