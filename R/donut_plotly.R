
#' Create an Interactive Donut Chart using Plotly
#'
#' This function generates a donut chart using Plotly, based on the specified value and label columns from a data frame.
#'
#' @param df A data frame containing the data to be visualized.
#' @param value_col A string specifying the column in `df` that contains numerical values for the chart.
#' @param label_col A string specifying the column in `df` that contains labels for the chart.
#' @param chart_title A string specifying the title of the chart (default: `NULL`).
#' @param space_size A numeric value specifying the size of the hole in the donut chart (default: `0.65`).
#'
#' @return A Plotly object representing the donut chart.
#' @import plotly
#'
#' @export
#'
#' @examples
#' # do not run
#' library(plotly)
#' test_df <- data.frame(
#'   category = c("A", "B", "C"),
#'   values = c(30, 50, 20)
#' )
#' DonutPlotly(test_df, value_col = "values", label_col = "category", chart_title = "Example Donut Chart")
DonutPlotly <- function(df, value_col, label_col, chart_title = NULL, space_size = 0.65) {
  fig <- df %>%
    plot_ly(labels = ~.data[[label_col]], values = ~.data[[value_col]]) %>%
    add_pie(hole = space_size) %>%
    layout(title = chart_title,
           showlegend = TRUE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  return(fig)
}
