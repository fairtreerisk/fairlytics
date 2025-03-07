#' WindowPanel
#'
#' @description
#' Wraps multiple collapsible sections in a draggable, scrollable panel for displaying interactive content.
#'
#' @param panel_layers (HTML widget) A list of HTML elements representing individual panel sections.
#' @param title (string) A title for the window panel, displayed at the top.
#' @param window_position (numeric vector) A numeric vector of length 4 specifying the top, right, bottom, and left positioning of the panel.
#' @param width (string, default = "auto") The width of the window panel. Defaults to "auto".
#' @param height (string, default = "auto") The height of the window panel. Defaults to "auto".
#'
#' @import htmltools
#' @import shiny
#'
#' @return An HTML element containing the draggable, scrollable window panel.
WindowPanel <- function(panel_layers, title, window_position, width= "auto", height = "auto") {
  wndw_panel <- absolutePanel(id = "controls", fixed = TRUE,
                              draggable = TRUE,
                              top = window_position[1],
                              right = window_position[2],
                              bottom = window_position[3],
                              left = window_position[4],
                              width = width,
                              height = height,
                              style = "overflow-y:hidden; max-height: 1500px; opacity: 0.75; z-index: 10000",
                              h6(strong(title)),
                              panel_layers)

  return(wndw_panel)
}
