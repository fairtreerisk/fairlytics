#' RelativeWeightBarChart
#'
#' @description
#' This function generates a flipped bar graph comprising three vertically aligned bar charts.
#' It is particularly useful for comparing two funds, with the third chart showing the relative
#' difference between the two. The bars are colored to represent the funds, and values are annotated
#' on the bars for easier interpretation.
#'
#' @param df A data frame containing the data to be visualized.
#' @param instrument_col (string) specifying the column name for the instrument names
#' (used on the x-axis).
#' @param weight_col (string) specifying the column name for the weights (used on the y-axis).
#' @param fund_label_col (string) specifying the column name for fund labels (used for
#' grouping and facetting).
#' @param txt_abv_grph (numeric) controlling the distance from the bar graph to the annotated
#' value (default: `0.05`).
#' @param instr_txt_size (numeric) controlling the font size of the instrument text (default: `12.5`).
#' @param axis_txt_size (numeric) controlling the font size of axis text (default: `12.5`).
#' @param num_size (numeric) controlling the size of the numbers displayed next to the bars
#' (default: `4`).
#' @param percent (logical) indicating whether the y-axis should display percentages (`TRUE`) or
#' raw numbers (`FALSE`) (default: `TRUE`).
#'
#' @return A `plotly::ggplotly` object containing the flipped bar graph.
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom plotly ggploty
#'
#' @examples
#' # Example data
#' test_df <- data.frame(
#'   instrument = c("Instrument A", "Instrument B", "Instrument C"),
#'   weight = c(0.1, -0.05, 0.2),
#'   fund_label = c("Fund 1", "Fund 1", "Fund 2")
#' )
#' RelativeWeightBarChart(test_df, "instrument", "weight", "fund_label", txt_abv_grph = 0.02)
#'
RelativeWeightBarChart <- function(df,
                           instrument_col,
                           weight_col,
                           fund_label_col,
                           txt_abv_grph = 0.05,
                           instr_txt_size = 12.5,
                           axis_txt_size = 12.5,
                           num_size = 4,
                           percent = TRUE) {

  .inputColumnChecker(df, instrument_col, weight_col, fund_label_col)

  y_scale_labels <- if (percent) scales::percent else waiver()

  p <- ggplot(df, aes(x = reorder(.data[[instrument_col]], .data[[weight_col]]),
                      y = .data[[weight_col]],
                      fill = .data[[fund_label_col]])) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    scale_y_continuous(labels = y_scale_labels) +
    theme_bw() +
    scale_fill_manual(values = c("#FF8200", "#34AA9D", "#6584A4")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = axis_txt_size),
      axis.text.y = element_text(hjust = 1, size = instr_txt_size),
      strip.text.x = element_text(size = 12.5, colour = "darkorange4"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(10, 10, 25, 25),
      legend.position = "none"
    ) +
    geom_text(aes(
      label = if (percent) paste0(round(100 * .data[[weight_col]], 1), "%") else round(.data[[weight_col]], 2),
      y = if_else(.data[[weight_col]] > 0, .data[[weight_col]] + txt_abv_grph, .data[[weight_col]] - txt_abv_grph)
    ), size = num_size) +
    coord_flip() +
    facet_wrap(~ .data[[fund_label_col]]) +
    ylab("") +
    xlab("")

  return(ggplotly(p))
}
