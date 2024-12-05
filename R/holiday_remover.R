#' HolidayRemover
#'
#' @description
#' The `HolidayRemover` function cleanses a dataset by removing weekends and holidays.
#' It generates a custom business day calendar based on the minimum and maximum dates in the dataset,
#' using a specified holiday calendar. This is particularly useful for analyzing time series data
#' that should only include business days.
#'
#' @param data A data frame containing a `Date` column.
#' This column is required and must represent the dates to evaluate.
#' @param date_col A string specifying the name of the column in `data` that contains the date values.
#' The column should be of class `Date` or convertible to it.
#' @param calendar_name A string specifying the name of the holiday calendar to use.
#' The default is `"SouthAfrica"`.
#' You can choose any valid calendar supported by the `bizdays` package.
#'
#' @return data-frame excluding holidays and weekends based on the calendar provided
#' @export
#'
#' @import dplyr
#' @import stringr
#' @import bizdays
#' @import purrr
#' @import tibble
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example dataset
#' data_df <- data.frame(
#'   Date = seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by = "day"),
#'   Value = rnorm(10)
#' )
#'
#' # Removing weekends and South African holidays from the dataset
#' cleaned_data <- HolidayRemover(data = data_df, "Date", calendar_name = "SouthAfrica")
#' print(cleaned_data)
HolidayRemover <- function(data, date_col, calendar_name="SouthAfrica") {

  .inputColumnChecker(df = data, date_col)

  min_date <- min(data[[date_col]])
  max_date <- max(data[[date_col]])

  bizdays::load_quantlib_calendars(ql_calendars = calendar_name,
                                   from = min_date,
                                   to = max_date)

  gen_calendar <- paste0("QuantLib/", calendar_name)

  bizdays::create.calendar(name = "MyCalendar",
                           start.date = min_date,
                           end.date = max_date,
                           weekdays = c("saturday", "sunday"))

  biz_days_df <- tibble(Date = bizseq(min_date,max_date, "MyCalendar")) %>%
    mutate('is_bizday' = map_lgl(.data[[date_col]], ~ is.bizday(.x, cal = gen_calendar))) %>%
    filter(is_bizday == T) %>%
    select(-'is_bizday')

  out <- left_join(biz_days_df, data, by = "Date")

  return(out)
}
