#' WindowPanel
#'
#' @description
#' Wraps multiple collapsible sections in a draggable, scrollable panel for displaying interactive content.
#'
#' @param panel_layers (HTML widget) A list of HTML elements representing individual panel sections.
#' @param title (string) A title for the window panel, displayed at the top.
#'
#' @import htmltools
#' @import shiny
#'
#' @return An HTML element containing the draggable, scrollable window panel.
WindowPanel <- function(panel_layers, title) {
  wndw_panel <- absolutePanel(id = "controls", fixed = TRUE,
                              draggable = TRUE, top = 115, left = 30, right = 40,
                              bottom = "auto", width = '90%', height = '150%',
                              style = "overflow-y:scroll; max-height: 2500px; opacity: 0.75; z-index: 10000",
                              h6(strong(title)),
                              panel_layers)

  return(wndw_panel)
}

