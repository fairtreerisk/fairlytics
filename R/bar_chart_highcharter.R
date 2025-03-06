#' Generate a Highcharts Bar or Column Chart
#'
#' This function creates a bar or column chart using the `highcharter` package. It supports stacked or grouped bars,
#' allows ordering of bars, and provides customization options for axis labels, title, and text styling.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x_col A string specifying the column name for the x-axis.
#' @param y_col A string specifying the column name for the y-axis.
#' @param color_col A string specifying the column name for grouping bars by color.
#' @param bars_type A character string indicating the type of bars: either `"stack"` (default) or `"group"`.
#' @param x_label A string specifying the label for the x-axis (default is an empty string).
#' @param y_label A string specifying the label for the y-axis (default is an empty string).
#' @param order_bars Logical, whether to order bars based on their y-values (default: TRUE).
#' @param ascending Logical, whether to order bars in ascending order (default: FALSE, meaning descending order).
#' @param show_bar_text Logical, whether to display bar values as text on the bars (default: FALSE).
#' @param chart_title A string specifying the chart title (default is NULL).
#' @param chart_flip Logical, whether to flip the chart (bar chart instead of column chart) (default: FALSE).
#' @param axis_text_size Numeric, font size for axis text labels (default: 12).
#' @param num_abv_grph_size Numeric, font size for bar text labels if `show_bar_text` is TRUE (default: 4).
#' @param txt_space_abv_grh Numeric, spacing adjustment for text above bars (default: 0.005).
#'
#' @return A Highcharts object representing the bar or column chart.
#' @import dplyr
#' @import highcharter
#' @importFrom rlang sym .data
#' @export
BarCharter <- function(df,
                       x_col,
                       y_col,
                       color_col,
                       bars_type = c("stack", "group"),
                       x_label="",
                       y_label="",
                       order_bars = TRUE,
                       ascending = FALSE,
                       show_bar_text = FALSE,
                       chart_title = NULL,
                       chart_flip = FALSE,
                       axis_text_size = 12,
                       num_abv_grph_size = 4,
                       txt_space_abv_grh = 0.005) {

  .inputColumnChecker(df = df, x_col, y_col, color_col)

  bars_type <- match.arg(bars_type)  # Validate bars_type argument

  stacking_option <- ifelse(bars_type == "stack", "normal", bars_type)

  # Set chart type: column or bar
  chart_type <- ifelse(chart_flip, "bar", "column")

  x_order <- df %>%
    group_by(!!sym(x_col)) %>%
    summarise(total = sum(!!sym(y_col), na.rm = TRUE), .groups = "drop") %>%
    arrange(if (order_bars) if (ascending) total else desc(total)) %>%
    pull(!!sym(x_col))

  # Apply factor levels order
  df <- df %>%
    mutate(!!sym(x_col) := factor(.data[[x_col]], levels = x_order))

  p <- highcharter::hchart(df, chart_type,
                           hcaes(x = .data[[x_col]],
                                 y = .data[[y_col]],
                                 group = .data[[color_col]])) %>%
    hc_xAxis(title = list(text = x_label)) %>%
    hc_yAxis(title = list(text = y_label)) %>%
    hc_title(text = chart_title) %>%
    hc_plotOptions(column = list(stacking = stacking_option),
                   bar = list(stacking = stacking_option))

  # Add text labels
  if (show_bar_text) {
    p <- p %>%
      hc_plotOptions(series = list(
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%",
          style = list(fontSize = paste0(num_abv_grph_size, "px"))
        )
      ))
  }

  return(p)
}
