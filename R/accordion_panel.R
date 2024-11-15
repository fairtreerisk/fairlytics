#' AccordionPanel
#'
#' @description
#' Creates a collapsible UI panel for displaying grouped bar charts in an interactive application.
#' Each panel section is collapsible and can contain any HTML content, making it ideal for organizing
#' various interactive elements compactly and accessibly.
#'
#' @param df A data frame containing the data to be displayed in the panel.
#' @param value_col (string) The column in `df` that contains the values to be displayed in the chart.
#' @param chart_title (string) An optional title for the chart, displayed at the top of the panel.
#' @param color (hex or string) A color value (in hex format, default is "#FF8200") to define the bar chart's fill color.
#'
#' @return An HTML element containing the collapsible panel with grouped bar charts based on `ggplot2`.
#'
#' @export
#'
#' @import dplyr
#' @import uuid
#' @import shiny
#' @import ggplot2
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom magrittr %>%
#'
#' @examples
#' AccordionPanel(mydata, "amount", "group_value", "Collapsible Panel Testing")
AccordionPanel <- function(df, value_col, grouping_col, chart_title=NULL, color="#FF8200") {

  .inputColumnChecker(df, value_col, grouping_col)

  df2 <- df %>% arrange(.data[[grouping_col]])
  group_names <- sort(unique(df2[[grouping_col]]))
  html_layers <- list()

  for (grp in group_names) {

    panel_idx <- paste0("box", UUIDgenerate()) # Generate a unique ID
    group_data <- df2 %>% filter(.data[[grouping_col]] == grp)
    bar_chart <- BarChartPlot(df = group_data, x_col = grouping_col, y_col = value_col, color = color)

    tmp_layer <- div(
      HTML(paste0('<button data-toggle="collapse" data-target="#', panel_idx,
                  '" class="btn btn-info btn-block">', grp, '</button>')),
      tags$div(id = panel_idx, class = "collapse", bar_chart)
    )

    html_layers <- append(html_layers, list(tmp_layer))
  }

  wndw_layout <- WindowPanel(panel_layers = html_layers, title = chart_title)

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
BarChartPlot <- function(df, x_col, y_col, width = 0.5, color = "#FF8200") {

  .inputColumnChecker(df, x_col, y_col)

  p <- ggplot(df, aes(x = reorder(.data[[x_col]], - .data[[y_col]]), y = .data[[y_col]])) +
    geom_bar(stat = "identity",
             position = "dodge",
             width = width,
             fill = color) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, size = 10.0)) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(10, 10, 12, 11))

  return(ggplotly(p))
}



