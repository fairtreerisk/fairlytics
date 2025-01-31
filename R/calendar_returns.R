#' Create a Calendar Returns Table
#'
#' This function calculates calendar returns based on a given dataset, organizing
#' the returns by year and month, and computing the Year-To-Date (YTD) return.
#'
#' @param df A data frame containing the date and return columns.
#' @param date_col A string specifying the name of the date column in `df`.
#' @param return_col A string specifying the name of the return column in `df`.
#'
#' @return A data frame where each row represents a year, with monthly returns
#'         as columns and a YTD return column.
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#'
#' @importFrom magrittr %>%
#'
#' @export
GenerateCalendarReturn <- function(df, date_col, return_col) {

  .inputColumnChecker(df, date_col, return_col) # check input column names

  df[[date_col]] <- as_date(df[[date_col]])
  calender_data <- df %>%
    mutate(Year = lubridate::year(!!sym(date_col)),
           Month = base::month.abb[lubridate::month(!!sym(date_col))]) %>%
    select(-!!sym(date_col)) %>%
    group_by(Year) %>%
    mutate("YTD" = prod(1 + !!sym(return_col)) - 1) %>%  # calculating YTD return
    ungroup() %>%
    select(Year,Month, !!sym(return_col),YTD) %>%
    pivot_wider(names_from = Month, values_from = !!sym(return_col)) %>%
    select(Year, all_of(month.abb), YTD) %>%
    arrange(Year)

  return(calender_data)
}
