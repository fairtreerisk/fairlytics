
#' ActiveWeightsDeviationChart
#'
#' @description
#' Function creates a horizontal bar chart (mirrored layout) to visualize active weights
#' for instruments. The chart highlights instruments based on their active weights and adds
#' red dashed horizontal lines at specified `min_band` and `max_band` values for reference.
#'
#' @param df A data frame containing the data to be plotted.
#' @param instrument_col (string) specifying the column name for the instruments of interest.
#' @param active_weight_col (string) specifying the column name for the active weights.
#' @param min_band (numeric) indicating the lower reference line on the y-axis (default: `-0.0005`).
#' @param max_band (numeric) indicating the upper reference line on the y-axis (default: `0.0005`).
#'
#' @return A `plotly::ggplotly` object representing the mirrored bar chart.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom scales percent
#' @importFrom stats reorder
#'
#' @examples
#' # Example data
#' test_df <- data.frame(
#'   instrument = c("Instrument A", "Instrument B", "Instrument C"),
#'   active_weight = c(0.002, -0.001, 0.0003)
#' )
#' ActiveWeightsDeviationChart(test_df,
#' instrument_col = "instrument",
#' active_weight_col = "active_weight")
ActiveWeightsDeviationChart <- function(df,instrument_col,active_weight_col,
                                        min_band = -0.0005, max_band = 0.0005){

  .inputColumnChecker(df, instrument_col, active_weight_col)

  p <- ggplot(df, aes(x = reorder(.data[[instrument_col]], -.data[[active_weight_col]]),
                      y = .data[[active_weight_col]])) +
    geom_bar(stat = "identity", position = "dodge", fill = "#49362F") +
    scale_y_continuous(labels = scales::percent) +
    theme_light() +
    labs(x = "", y = "Weight") +
    geom_hline(yintercept = max_band, linetype = "dashed", color = "red", size = 0.3) +
    geom_hline(yintercept = min_band, linetype = "dashed", color = "red", size = 0.3) +
    coord_flip()

  return(ggplotly(p))

}
