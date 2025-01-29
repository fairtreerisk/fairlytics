
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
#' load_quantlib_calendars(ql_calendars = "SouthAfrica", from = as.Date("2023-01-01"), to = as.Date("2024-12-31"))
#'
#' # Create example business dates
#' biz_dates <- bizdays::bizseq(from = as.Date("2023-01-01"), to = as.Date("2024-12-31"), cal = "QuantLib/SouthAfrica")
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
#' ComputeReturnMetrics(cumulative_df, "Date", as.Date("2024-01-31"), value_cols = c("Return1", "Return2"))
#' }
#'
#' @import dplyr
#' @import lubridate
#' @import bizdays
#' @export
ComputeReturnMetrics  <- function(cumulative_df,date_col,current_date,
                                      value_cols = NULL,
                                      calendar = "QuantLib/SouthAfrica") {

  output_tbl <- data.frame()
  min_date <- min(cumulative_df[[date_col]])
  max_date <- max(cumulative_df[[date_col]])

  load_quantlib_calendars(ql_calendars = str_remove_all(calendar,"QuantLib/"),
                          from = min_date, to = max_date)
  biz_dates <- bizdays::bizseq(from = min_date, to = max_date, cal = calendar)

  # Get MTD start date
  month_bgn_date <- floor_date(current_date, "month")
  last_month_end_date <- .getLastBizDate(month_bgn_date - days(1), biz_dates)

  # Get YTD start date
  ytd_start_date <- floor_date(current_date, "year")
  last_year_end_date <- .getLastBizDate(ytd_start_date - days(1), biz_dates)

  # MTD Return
  mtd_rtn <- cumulative_df %>%
    filter(!!sym(date_col) %in% c(last_month_end_date, current_date)) %>%
    arrange(!!sym(date_col)) %>%
    summarise(across(all_of(value_cols), ~ last(.) / first(.) - 1)) %>%
    mutate(Metrics = "Month-to-Date")

  # YTD Return
  ytd_rtn <- cumulative_df %>%
    filter(!!sym(date_col) %in% c(last_year_end_date, current_date)) %>%
    arrange(!!sym(date_col)) %>%
    summarise(across(all_of(value_cols), ~ last(.) / first(.) - 1)) %>%
    mutate(Metrics = "Year-to-Date")

  # Inception-to-Date Return
  inception_rtn <- cumulative_df %>%
    filter(!!sym(date_col) == current_date) %>%
    mutate(across(all_of(value_cols), ~ . / 100 - 1)) %>%
    mutate(Metrics = "Inception-to-Date")

  # Combine Results
  output_tbl <- bind_rows(mtd_rtn, ytd_rtn, inception_rtn) %>%
    select("Metrics", all_of(value_cols))

  return(output_tbl)
}
