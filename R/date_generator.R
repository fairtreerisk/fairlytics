#' Generate Business Days Between Two Dates
#'
#' This function generates a sequence of business days between a given start and end date
#' using a specified business calendar.
#'
#' @param bgn_date A `Date` object or a string in `YYYY-MM-DD` format representing the start date.
#' @param end_date A `Date` object or a string in `YYYY-MM-DD` format representing the end date.
#' @param calendar A string specifying the business calendar to use. Defaults to `"QuantLib/SouthAfrica"`.
#'
#' @details
#' The function relies on the `bizdays` package to calculate business days,
#' and it uses the QuantLib implementation for calendar definitions. Ensure that the calendar is
#' loaded with `load_quantlib_calendars` before calling this function.
#'
#' @return A tibble containing the sequence of business days between `bgn_date` and `end_date`.
#'
#' @examples
#' # Example usage:
#' generateBusinessDays("2025-01-01", "2025-01-31", calendar = "QuantLib/SouthAfrica")
#'
#' @import bizdays
#' @export
GenerateBusinessDays <- function(bgn_date, end_date, calendar = "QuantLib/SouthAfrica") {
  load_quantlib_calendars(ql_calendars = "SouthAfrica", from = bgn_date, to = end_date)
  dates <- bizdays::bizseq(bgn_date, end_date, cal = calendar) %>% as_tibble()

  return(dates)
}


