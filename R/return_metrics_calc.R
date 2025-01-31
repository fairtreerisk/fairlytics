
#' Calculate Summary Performance Metrics
#'
#' Calculate Month-to-Date (MTD), Year-to-Date (YTD), and Inception-to-Date (ITD)
#' performance returns based on a cumulative returns data frame. Ensures that
#' date-based calculations are aligned with business days from a specified calendar.
#'
#' @param cumulative_df A data frame containing cumulative return data.
#' @param date_col A string specifying the name of the column containing date values.
#' @param current_date A Date object representing the current date for the calculation.
#' @param value_cols A character vector specifying the columns for which performance
#'     returns should be calculated.
#' @param calendar A string specifying the business calendar to use. Defaults to `"QuantLib/SouthAfrica"`.
#'
#' @return A data frame containing MTD, YTD, and ITD performance metrics.
#'
#' @details
#' The function ensures that the last month-end and last year-end dates are valid
#' business dates according to the specified business calendar. The returns are calculated as:
#' - **MTD Return**: Performance from the last business day of the previous month to `current_date`.
#' - **YTD Return**: Performance from the last business day of the previous year to `current_date`.
#' - **ITD Return**: Performance since inception (assumed to be 100 at the beginning).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lubridate)
#' library(bizdays)
#'
#' # Load business calendar
#' load_quantlib_calendars(ql_calendars = "SouthAfrica",
#' from = as.Date("2023-01-01"), to = as.Date("2024-12-31"))
#'
#' # Create example business dates
#' biz_dates <- bizdays::bizseq(from = as.Date("2023-01-01"),
#' to = as.Date("2024-12-31"), cal = "QuantLib/SouthAfrica")
#'
#' # Sample cumulative returns dataset
#' set.seed(123)
#' cumulative_df <- data.frame(
#'   Date = biz_dates,
#'   Return1 = runif(length(biz_dates), 95, 105),
#'   Return2 = runif(length(biz_dates), 90, 110)
#' )
#'
#' # Run function
#' ComputeReturnMetrics(cumulative_df, "Date", as.Date("2024-01-31"),
#' value_cols = c("Return1", "Return2"))
#' }
#'
#' @import dplyr
#' @import lubridate
#' @import bizdays
#' @import tidyr
#' @import RQuantLib
#' @export
ComputeReturnMetrics <- function(cumulative_df, date_col, current_date,
                                 value_cols = NULL,
                                 calendar = "QuantLib/SouthAfrica") {

  .inputColumnChecker(cumulative_df, date_col,value_cols)

  min_date <- min(cumulative_df[[date_col]])
  max_date <- max(cumulative_df[[date_col]])

  load_quantlib_calendars(ql_calendars = str_remove_all(calendar, "QuantLib/"),
                          from = min_date, to = max_date)
  biz_dates <- bizdays::bizseq(from = min_date, to = max_date, cal = calendar)

  if (nrow(cumulative_df) == 1) {
    out <- cumulative_df %>%
      mutate(across(all_of(value_cols), ~ . / 100 - 1)) %>%
      crossing(Metrics = c("Month-to-Date", "Year-to-Date", "Inception-to-Date"))
    return(out)
  }

  get_last_biz_date <- function(date) {
    valid_dates <- biz_dates[biz_dates <= date]
    if (length(valid_dates) > 0) {
      return(max(valid_dates, na.rm = TRUE))
    } else {
      return(NA)  # Return NA if no valid dates exist
    }
  }

  adjust_to_biz_date <- function(date) {
    if (is.na(date)) return(min_date)  # Default to min_date if no valid business date
    while (!(date %in% biz_dates) && date > min_date) {
      date <- date - days(1)
    }
    max(date, min_date)
  }

  last_month_end <- adjust_to_biz_date(get_last_biz_date(floor_date(current_date, "month") - days(1)))
  last_year_end <- adjust_to_biz_date(get_last_biz_date(floor_date(current_date, "year") - days(1)))

  calc_return <- function(start_date, label) {
    cumulative_df %>%
      filter(!!sym(date_col) %in% c(start_date, current_date)) %>%
      arrange(!!sym(date_col)) %>%
      summarise(across(all_of(value_cols), ~ last(.) / first(.) - 1),
                Metrics = label)
  }

  out <- bind_rows(
    calc_return(last_month_end, "Month-to-Date"),
    calc_return(last_year_end, "Year-to-Date"),
    cumulative_df %>%
      filter(!!sym(date_col) == current_date) %>%
      mutate(across(all_of(value_cols), ~ . / 100 - 1), Metrics = "Inception-to-Date")
  ) %>%
    select("Metrics", all_of(value_cols))

  return(out)
}
