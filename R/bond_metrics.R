
#' Bond Metrics
#'
#' @description
#' The `BondMetrics` function calculates and augments bond metrics from a given data frame.
#' It computes current yield, weighted metrics (Modified Duration, Credit Spread Duration, and Current Yield)
#' based on specified weights, and categorizes bonds into maturity year ranges.
#'
#' @param Cube A data frame containing bond data. It must include the following columns:
#' - `Coupon`: The coupon rate of the bond.
#' - `Price`: The current price of the bond.
#' - `ModifiedDuration`: The modified duration of the bond.
#' - `CreditSpreadDuration`: The credit spread duration of the bond.
#' - `weight`: The weight associated with each bond.
#' - `Maturity`: The maturity date of the bond.
#' - `report_date`: The date of the report.
#'
#' @return A data frame with the following additional columns:
#' - `CurrentYield`: The current yield of the bond.
#' - `Weighted_MD`: The weighted modified duration of the bond.
#' - `Weighted_CS`: The weighted credit spread duration of the bond.
#' - `Weighted_CY`: The weighted current yield of the bond.
#' - `YearsToMaturity`: The number of years until the bond matures.
#' - `YearSplit`: A categorical variable indicating the maturity range of the bond.
#' @export
#'
#' @examples
#' # Calculating bond metrics for the given data frame
#' BondMetrics(credit_df)
BondMetrics <- function(Cube){

  Metrics <- Cube %>%
    mutate(CurrentYield = (Coupon * 100) / Price,
           Weighted_MD = ModifiedDuration * weight,
           Weighted_CS = CreditSpreadDuration * weight,
           Weighted_CY = (CurrentYield * weight) * 100) %>%
    mutate(YearsToMaturity = (Maturity - report_date) / 365) %>%
    mutate_at(.vars = vars(`YearsToMaturity`), list(~ifelse(is.na(.), 0, .))) %>%
    mutate("YearSplit" = case_when(YearsToMaturity <= 1, "0 to 1 year",
                                   (YearsToMaturity > 3 & YearsToMaturity <= 7), "1 to 3 years",
                                   (YearsToMaturity > 7 & YearsToMaturity <= 12), "7 to 12 years",
                                   YearsToMaturity > 12, "Above 12 years"))

  return(Metrics)
}
