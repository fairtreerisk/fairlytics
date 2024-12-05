
#' InvestmentLookThrough
#'
#' @description
#' The `InvestmentLookThrough` function provides a deeper analysis of the underlying fund investments within a portfolio.
#' Rather than viewing the fund investment as a single entity,
#' this function breaks down the fund investment to examine the individual securities or holdings it contains.
#' This approach offers a more granular understanding of the fund's actual exposure to different assets.
#' The exposure values are then adjusted or scaled to align with the overall value of the fund investment within a portfolio.
#'
#' @param fund_holdings A data frame representing the exposure of the fund holdings.
#' It must include the `zar_position_value` and `weight` columns.
#' @param underlying_holdings A data frame representing the underlying securities or holdings within the fund.
#' It must include a column matching the `pos_value_col` parameter.
#' @param lookup_fund_ticker specifying the ticker of the underlying fund within the `fund_holdings` data frame.
#' For example, `"ABC SJ Equity"`.
#' @param pos_value_col A string representing the column name for position values in both data frames.
#' For example, `"zar_position_value"`.
#' @param tickers_col A string representing the column name for fund tickers in the `fund_holdings` data frame.
#' For example, `"instrument_name"`.
#' @param wgt_col A string representing the column name for weights in the `fund_holdings` data frame.
#' For example, `"weight"`.
#'
#' @return A data frame containing the rescaled look-through data,
#' with values adjusted based on the overall fund investment.
#' The output includes the rescaled position values and updated weights for the underlying securities.
#' @export
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example data for overall fund holdings
#' fund_holdings <- data.frame(
#'   instrument_name = c("ABC SJ Equity", "XYZ Bond"),
#'   zar_position_value = c(1000000, 500000),
#'   weight = c(0.6, 0.4)
#' )
#'
#' # Example data for underlying holdings of the fund "ABC SJ Equity"
#' underlying_holdings <- data.frame(
#'   security_name = c("Security A", "Security B", "Security C"),
#'   zar_position_value = c(600000, 300000, 100000)
#' )
#'
#' # Performing the investment look-through analysis
#' result <- InvestmentLookThrough(
#'   fund_holdings = fund_holdings,
#'   underlying_holdings = underlying_holdings,
#'   lookup_fund_ticker = "ABC SJ Equity",
#'   pos_value_col = "zar_position_value",
#'   tickers_col = "instrument_name",
#'   wgt_col = "weight"
#' )
#'
#' print(result)
InvestmentLookThrough <- function(fund_holdings,
                                  underlying_holdings,
                                  lookup_fund_ticker,
                                  pos_value_col,
                                  tickers_col,
                                  wgt_col){

  holding_AUM <- sum(fund_holdings[[pos_value_col]], na.rm = T)

  allocation_wgt <- fund_holdings %>%
    filter(.data[[tickers_col]] %in% lookup_fund_ticker) %>%
    pull(.data[[wgt_col]])

  if (is_empty(allocation_wgt)) {
    cli::cli_abort("Investment Ticker '{lookup_fund_ticker}' not found in the holding exposure table.")
  }

  allocated_aum <- round(holding_AUM * allocation_wgt,3)

  underlying_aum <- sum(underlying_holdings[[pos_value_col]], na.rm = TRUE)

  # rescaling the fund investment
  rescaled_df <- underlying_holdings %>%
    mutate('temp_wgt' := .data[[pos_value_col]]/underlying_aum,
           temp_position_value = temp_wgt * allocated_aum) %>%
    select(-any_of(c(!!pos_value_col, !!wgt_col))) %>%
    rename(!!pos_value_col := 'temp_position_value',
           !!wgt_col := 'temp_wgt') %>%
    mutate('weight' = .data[[pos_value_col]]/holding_AUM)


  # Validate if rescaled AUM matches the allocated value
  isSame <- round(sum(rescaled_df[[pos_value_col]], na.rm = TRUE), 0) == round(allocated_aum, 0)

  if (!isSame) {
    cli::cli_alert_info("Rescaled AUM does not match the invested value.")
  }

  return(rescaled_df)

}
