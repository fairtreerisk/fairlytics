
#' SeparateFundsERA
#'
#' @description
#' The `SeparateFundsERA` function processes a dataset to filter and format data for a specific fund.
#' It selects relevant columns, replaces missing or empty values in the `Sector` column with "Cash",
#' and arranges the data by `Ticker`. The resulting data frame provides key financial information
#' for the selected fund.
#'
#' @param data_tbl A data frame or tibble containing financial data. It should include columns:
#'        `Fund`, `Date`, `Stock`, `Ticker`, `Sector`, `currency`, `Price`, `Quantity`,
#'        `Market Value`, `Weight`, and `Fx Close`.
#' @param fund_name A character vector specifying the names of the funds to include in the analysis.
#'        The function will only include rows corresponding to these fund names.
#'
#' @return A data frame with the following columns:
#' - `Date`: The date of the record.
#' - `Fund`: The name of the fund.
#' - `Stock`: The stock name.
#' - `Ticker`: The stock ticker symbol.
#' - `Sector`: The sector to which the stock belongs (with empty sectors replaced by "Cash").
#' - `currency`: The currency of the investment.
#' - `Price`: The price of the stock.
#' - `Quantity`: The quantity of the stock held.
#' - `Market Value`: The market value of the stock.
#' - `Weight`: The weight of the stock within the fund.
#' - `Fx Close`: The foreign exchange close value.
#'
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
SeparateFundsERA <- function(data_tbl,fund_name){

  df <- data_tbl %>%
    filter(Fund %in% fund_name) %>%
    select(Date, Fund, Stock, Ticker, Sector, currency, Price, Quantity,
           'Market Value', Weight, 'Fx Close') %>%
    mutate(Sector = case_when(Sector == "" ~ "Cash", TRUE ~ Sector)) %>%
    arrange(Ticker)

  return(df)
}
