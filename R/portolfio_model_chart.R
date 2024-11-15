
#' PortfolioModelTable
#'
#' @description
#' This function returns a formatted and styled `DT` table highlighting the over-weights
#' and under-weights in a portfolio compared to model weights. The table includes color
#' formatting to visually represent the differences.
#'
#' @param df A data frame containing the input data.
#' @param mkt_value (string) specifying the name of the column containing market values.
#' @param port_wgt (string) specifying the name of the column containing portfolio weights or holdings.
#' @param model_wgt (string) specifying the name of the column containing model allocation weights.
#' @param active_wgt (string)specifying the name of the column containing active weights
#' (difference between portfolio weight and model weight).
#'
#' @return A `DT` object styled for easy visualization.
#'
#' @export
#'
#' @import DT
#' @import htmlwidgets
#'
#' @examples
#' # Example usage:
#' test_df <- data.frame(
#'   account_code = c('abc', 'xyz', 'test0001')
#'   account_code = c('Equities', 'Bonds', 'Commodities')
#'   mkt_value = c(300, 200, 100),
#'   port_wgt = c(0.5, 0.3, 0.2),
#'   model_wgt = c(0.4, 0.4, 0.2),
#'   active_wgt = c(0.1, -0.1, 0.0)
#' )
#' PortfolioModelTable(
#'   df = test_df,
#'   mkt_value = "mkt_value",
#'   port_wgt = "port_wgt",
#'   model_wgt = "model_wgt",
#'   active_wgt = "active_wgt"
#' )
PortfolioModelTable <- function(df,mkt_value, port_wgt, model_wgt, active_wgt){

  .inputColumnChecker(df,mkt_value, port_wgt, model_wgt, active_wgt)

  dt <- datatable(df,
                  rownames = FALSE,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    columnDefs = list(list(
                      className = "dt-left",
                      targets = 1:2)
                      )
                    )
                  ) %>%
    formatCurrency(mkt_value, " ", interval = 3, mark = " ") %>%
    formatStyle(port_wgt,
                background = styleColorBar(range(df[[port_wgt]]),"darkturquoise"),
                backgroundSize = "28% 38%",
                backgroundRepeat = "no-repeat",
                backgroundPosition = "center") %>%
    formatStyle(model_wgt,
                background = styleColorBar(range(df[[model_wgt]]),"darkturquoise"),
                backgroundSize = "28% 38%",
                backgroundRepeat = "no-repeat",
                backgroundPosition = "center") %>%
    formatStyle(active_wgt,
                background = color_from_middle(df[[active_wgt]],'orange','red'),
                backgroundSize = "28% 38%",
                backgroundRepeat = "no-repeat",
                backgroundPosition = "center")

  return(dt)

}



color_from_middle <- function(data, color1,color2)
{
  max_val = max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
}

