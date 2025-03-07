#' AccordionPanel
#'
#' @description
#' Creates a collapsible UI panel for displaying grouped bar charts in an interactive application.
#' Each panel section is collapsible and can contain any HTML content, making it ideal for organizing
#' various interactive elements compactly and accessibly.
#'
#' @param df A data frame containing the data to be displayed in the panel.
#' @param value_col (string) The column in `df` that contains the values to be displayed in the chart.
#' @param grouping_col (string) The column that will be used to group the data for each panel.
#' @param underlying_bars_col (string) The column that represents the categories for the bars in the chart.
#' @param chart_title (string, optional) A title for the chart, displayed at the top of the panel.
#' @param position (character vector) A vector of 4 values specifying the position of the panel (`top`, `right`, `bottom`, `left`). Default is `c("auto", "auto", "auto", "auto")`.
#' @param width (character) The width of the panel. Default is `"auto"`.
#' @param height (character) The height of the panel. Default is `"auto"`.
#' @param color (string or hex) A color value (in hex format or named color). Default is `"#FF8200"`.
#' @param underlying_chart_height (numeric) The height of the individual bar charts in the panel. Default is `350`.
#'
#' @return An HTML element containing the collapsible panel with grouped bar charts based on `ggplot2`.
#'
#' @export
#'
#' @import dplyr
#' @import uuid
#' @import shiny
#' @import ggplot2
#' @importFrom uuid UUIDgenerate
#' @importFrom magrittr %>%
#'
#' @examples
#' library(shiny)
#'
#' # Example data
#' test_data <- data.frame(
#'   group_value = rep(c("Group A", "Group B", "Group C"), each = 3),
#'   category = rep(c("Category 1", "Category 2", "Category 3"), times = 3),
#'   amount = c(10, 20, 15, 25, 35, 30, 40, 50, 45)
#' )
#'
#' AccordionPanel(
#'   df = test_data,
#'   value_col = "amount",
#'   grouping_col = "group_value",
#'   underlying_bars_col = "category",
#'   chart_title = "Collapsible Bar Charts"
#' )
AccordionPanel <- function(df,
                            value_col,
                            grouping_col,
                            underlying_bars_col,
                            chart_title = NULL,
                            position = c("auto", "auto", "auto", "auto"),
                            width= "auto",
                            height = "auto",
                            color = "#FF8200",
                            underlying_chart_height = 350) {
  # .inputColumnChecker(df, value_col, grouping_col)
  df2 <- df %>% arrange(.data[[grouping_col]])
  group_names <- sort(unique(df2[[grouping_col]]))
  html_layers <- list()
  for (grp in group_names) {
    panel_idx <- paste0("box", UUIDgenerate())
    group_data <- df2 %>% filter(.data[[grouping_col]] ==
                                   grp)
    bar_chart <- BarChartPlot(df = group_data, x_col = underlying_bars_col,
                              y_col = value_col, color = color, height = underlying_chart_height)
    tmp_layer <- div(HTML(paste0("<button data-toggle=\"collapse\" data-target=\"#",
                                 panel_idx, "\" class=\"btn btn-info btn-block\">",
                                 grp, "</button>")),
                     tags$div(id = panel_idx,
                              class = "collapse",
                              bar_chart
                     )
    )

    html_layers <- append(html_layers, list(tmp_layer))
  }

  wndw_layout <- WindowPanel(panel_layers = html_layers,
                             title = chart_title,
                             window_position = position,
                             width = width,
                             height = height)
  return(wndw_layout)
}


#' BarChartPlot
#'
#' @description
#' Generates a bar chart using `ggplot2` and returns it as a plotly object for interactive use.
#'
#' @param df A data frame containing the data for the bar chart.
#' @param x_col (string) The column in `df` used for the x-axis.
#' @param y_col (string) The column in `df` used for the y-axis values.
#' @param width (numeric) Width of the bars (default is 0.5).
#' @param color (hex or string) Color of the bars in hex format (default is "#FF8200").
#'
#' @import dplyr
#' @import ggplot2
#'
#' @importFrom plotly ggplotly
#'
#' @return A `plotly` object containing the bar chart.
BarChartPlot <- function(df, x_col, y_col, width = 0.5, height = 250, color = "#FF8200") {

  # .inputColumnChecker(df, x_col, y_col)

  p <- ggplot(df, aes(x = reorder(.data[[x_col]], - .data[[y_col]]), y = .data[[y_col]])) +
    geom_bar(stat = "identity",
             position = "dodge",
             width = width,
             fill = color) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, size = 7)) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(10, 10, 12, 11))

  return(ggplotly(p, height = height))
}




