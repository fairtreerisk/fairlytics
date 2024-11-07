
#' Look Through Fund Investment
#'
#' @description
#' The `LookThroughFundInvestment` function provides a deeper analysis of the underlying fund investments within a fund.
#' Rather than viewing the fund investment as a single entity,
#' this function breaks down the fund investment to examine the individual securities or holdings it contains.
#' This approach offers a more granular understanding of the fund's actual exposure to different assets.
#' The exposure values are then adjusted or scaled to align with the overall value of the fund investment within a portfolio.
#'
#' @param holding_exposure A data frame representing the exposure of the fund holdings.
#' It must include the `zar_position_value` and `weight` columns.
#' @param lookup_fund_ticker specifying the ticker of the underlying fund within the `holding_exposure` data frame.
#' For example, `"ABC SJ Equity"`.
#' @param account_code representing the account code for the fund,
#' which is used to locate the specific fund data. For example, `"XYZELO01"`.
#'
#' @return A data frame containing the rescaled look-through data,
#' with values adjusted based on the overall fund investment.
#' The output includes the rescaled position values and updated weights for the underlying securities.
#' @export
#'
#' @examples
#' # Analyzing the underlying investments within a specific fund
#' LookThroughFundInvestment(holding_exposure = Holdings_data,
#'                                     lookup_fund_ticker = "ABC SJ Equity",
#'                                     account_code = "XYZELO01")
LookThroughFundInvestment <- function(holding_exposure,
                                      lookup_fund_ticker,
                                      account_code){

  holding_AUM <- sum(holding_exposure$zar_position_value, na.rm = T)

  allocation_wgt <- holding_exposure %>%
    filter(instrument_name == lookup_fund_ticker) %>%
    pull(weight)

  allocation_amt <- holding_AUM * allocation_wgt

  lookthrough_fund <- get(paste0(account_code,"_fund"))

  fund_value <- sum(lookthrough_fund$zar_position_value, na.rm = TRUE)

  # rescalling the fund investment
  look_through_rescale <- lookthrough_fund %>%
    mutate(weight = zar_position_value/fund_value,
           zar_position_value = weight * allocation_amt,
           weight = zar_position_value/holding_AUM)

  return(look_through_rescale)

}
