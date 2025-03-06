#' Bar Chart with Ordering using ggplot2
#'
#' This function generates a bar chart using ggplot2 with various customizable options, including bar ordering, text labels, and axis labels.
#' The bars can be ordered by their height (either ascending or descending) and the chart can be flipped if desired.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col A string specifying the column in `df` that contains the categorical variable for the x-axis.
#' @param y_col A string specifying the column in `df` that contains numerical values for the y-axis.
#' @param color_col A string specifying the column in `df` that determines the fill color of the bars.
#' @param bars_type A string indicating the type of the bars. Can be "stack" (default) or "group".
#' @param x_label A string specifying the label for the x-axis (default: `""`).
#' @param y_label A string specifying the label for the y-axis (default: `""`).
#' @param order_bars A logical value indicating whether to order the bars based on the `y_col` values (default: `TRUE`).
#' @param ascending A logical value indicating whether to order the bars in ascending order (`TRUE`) or descending order (`FALSE`, default).
#' @param chart_title A string specifying the title of the chart (default: `NULL`).
#' @param show_bar_text A logical value indicating whether to display text on top of the bars (default: `FALSE`).
#' @param chart_flip A logical value indicating whether to flip the chart coordinates (default: `FALSE`).
#' @param axis_text_size A numeric value specifying the size of the axis text (default: `12`).
#' @param num_abv_grph_size A numeric value specifying the size of the text above the bars (default: `4`).
#' @param txt_space_abv_grh A numeric value specifying the space above the bars for the text labels (default: `0.005`).
#'
#' @return A ggplot object representing the bar chart.
#' @export
#'
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' test_df <- data.frame(
#'   category = c("A", "B", "C"),
#'   values = c(0.3, 0.5, 0.2),
#'   color = c("red", "green", "blue")
#' )
#' BarChart_ggplot(test_df, x_col = "category", y_col = "values",color_col = "color",
#' chart_title = "Ordered Bar Chart", show_bar_text = TRUE)
BarChart_ggplot <- function(df,
                             x_col,
                             y_col,
                             color_col,
                             bars_type = c("stack", "group"),
                             x_label = "",
                             y_label = "",
                             order_bars = TRUE,
                             ascending = FALSE,
                             chart_title = NULL,
                             show_bar_text = FALSE,
                             chart_flip = FALSE,
                             axis_text_size = 12,
                             num_abv_grph_size = 4,
                             txt_space_abv_grh = 0.005) {

  .inputColumnChecker(df, x_col, y_col, color_col)

  bars_type <- match.arg(bars_type)  # Validate bars_type argument
  bars_type <- ifelse(bars_type == "group", "dodge", bars_type)

  x_order <- df %>%
    group_by(!!sym(x_col)) %>%
    reframe(!!sym(y_col) := sum(!!sym(y_col), na.rm = TRUE)) %>%
    arrange(if (order_bars) if (ascending) !!sym(y_col) else desc(!!sym(y_col))) %>%
    pull(x_col)

  # category factor levels
  df <- df %>%
    mutate(!!sym(x_col) := factor(.data[[x_col]], levels = x_order))

  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[color_col]])) +
    geom_bar(stat = "identity", position = bars_type, width = 0.5) +
    labs(title = chart_title, x = x_label, y = y_label) +
    theme_minimal(base_size = axis_text_size)

  # Add text labels on bars
  if (show_bar_text) {
    p <- p + geom_text(aes(
      label = paste0(round(100 * .data[[y_col]], 1)),
      y = .data[[y_col]] + if_else(.data[[y_col]] > 0, txt_space_abv_grh, -txt_space_abv_grh)
    ), size = num_abv_grph_size)
  }

  # Flip chart
  if (chart_flip) p <- p + coord_flip()

  return(p)
}

