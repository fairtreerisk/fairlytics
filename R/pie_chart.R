#' Create a Static Pie Chart with Customizable Gaps using ggplot2
#'
#' This function generates a pie chart with customizable gaps between segments using ggplot2. The chart is based on the specified value and color columns from a data frame.
#'
#' @param df A data frame containing the data to be visualized.
#' @param value_col A string specifying the column in `df` that contains numerical values for the chart.
#' @param color_col A string specifying the column in `df` that determines the fill color of the pie chart.
#' @param chart_title A string specifying the title of the chart (default: `NULL`).
#' @param gap_size A numeric value specifying the size of the gap between pie slices (default: `1`).
#' @param gap_color A string specifying the color of the gap (default: `"white"`).
#'
#' @return A ggplot object representing the pie chart.
#' @export
#'
#' @import ggplot2
#' @importFrom scales percent
#'
#' @examples
#' library(ggplot2)
#' test_data <- data.frame(
#'   category = c("A", "B", "C"),
#'   values = c(0.3, 0.5, 0.2)
#' )
#' PieChart(test_data, value_col = "values", color_col = "category", chart_title = "Example Pie Chart")
PieChart <- function(df, value_col, color_col, chart_title = NULL, gap_size = 1, gap_color = "white"){
  p <-  ggplot(df, aes(x = "" , y = .data[[value_col]], fill = .data[[color_col]])) +
    geom_bar(width = 1, size = gap_size, color = gap_color,stat = "Identity") +
    theme(legend.position = 'none') +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    coord_polar("y") +
    ggtitle(chart_title) +
    theme(axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
          plot.margin = margin(10,10,10,11)) +
    geom_text(aes(label = paste0(round(100 * .data[[value_col]], 1))), position = position_stack(vjust = 0.5) , size = 4) +
    theme(plot.title = element_text(size = 18))

  return(p)
}
