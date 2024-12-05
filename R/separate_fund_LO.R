
#' SeparateFundsLO
#'
#' @description
#' The `SeparateFundsLO` function processes a dataset to filter and summarize fund data.
#' It filters the data by specific fund names and report date, then groups the data by date and sector.
#' The function calculates the sum of the `weight` column for each sector within the specified date,
#' returning a summarized data frame.
#'
#' @param data_tbl A data frame or tibble containing the fund data. It should include columns
#'        `fund_name`, `date`, `weight`, and `sector`.
#' @param fund_name A character vector specifying the names of the funds to include in the analysis.
#' @param report_date A date value specifying the report date for filtering the data.
#'
#' @return A data frame with columns `date`, `sector`, and the sum of `weight` for each sector on the specified date.
#'         The result is grouped by `date` and `sector`.
#'
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
SeparateFundsLO <- function(data_tbl,fund_name,report_date){
  df <- data_tbl %>%
    filter(fund_name %in% fund_name, date == report_date) %>%
    select(date, weight, sector) %>%
    group_by(date, sector) %>%
    summarise_all(~sum(weight), na.rm = T) %>%
    ungroup()

  return(df)
}
