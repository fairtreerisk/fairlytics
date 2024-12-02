
#' AssetClassAllocationTable
#'
#' @description
#' This function generates a heatmap to provide a quick overview of asset allocation concentrations
#' across various funds or accounts and asset classes. Higher values are highlighted using the
#' specified high color (`h_color`, default: `darkorange3`), while lower values use the low color
#' (`l_color`, default: `honeydew1`), illustrating the intensity of allocation for each combination.
#'
#' @param df A data frame containing the data to be visualized.
#' @param x_col (string) specifying the column name for the x-axis (e.g., fund names).
#' @param y_col (string) specifying the column name for the y-axis (e.g., asset classes).
#' @param value_col (string) specifying the column name for the values to be displayed and
#' used for heatmap coloring.
#' @param chart_title (string) specifying the title of the chart.
#' @param l_color (string) specifying the low-end color for the heatmap gradient
#' (default: `"honeydew1"`).
#' @param h_color (string) specifying the high-end color for the heatmap gradient
#' (default: `"darkorange3"`).
#'
#' @return A heatmap as a `plotly::ggplotly` object.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#'
#' @examples
#' # Example data
#' test_df <- data.frame(
#'   fund = rep(c("Fund A", "Fund B", "Fund C"), each = 3),
#'   asset_class = rep(c("Equity", "Fixed Income", "Cash"), 3),
#'   allocation = runif(9, 0, 1)
#' )
#'
#' AssetClassAllocationTable(
#'   df = test_df,
#'   x_col = "fund",
#'   y_col = "asset_class",
#'   value_col = "allocation",
#'   chart_title = "Asset Allocation Heatmap"
#' )
AssetClassAllocationTable <- function(df, x_col, y_col, value_col, chart_title,
                                      l_color="honeydew1", h_color="darkorange3"){

  .inputColumnChecker(df, x_col, y_col, value_col) # check input column names

  to_plot <- df %>% filter(!is.na(.data[[value_col]]))

  p <- ggplot(to_plot, aes(x = .data[[x_col]], y = .data[[ y_col]] )) +
    geom_tile(aes(fill = .data[[value_col]])) +
    theme_bw() +
    scale_fill_gradient(low = l_color, high = h_color) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12.5),
          legend.position = "none") +
    theme(axis.text.y = element_text(hjust = 1, size = 12.0),
          legend.position = "none",
          plot.margin = margin(10,10,45,45)) +
    xlab("") +
    ylab("") +
    geom_text(aes(label = paste0(round(100 * .data[[value_col]], 0))), size = 3.5) +
    ggtitle(chart_title) +
    theme(plot.title = element_text(size = 18))

  return(ggplotly(p))
}




