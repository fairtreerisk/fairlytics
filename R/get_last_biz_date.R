#' Get the Last Business Date
#'
#' This function finds the most recent business date before or equal to a given date
#' according to a specified business calendar.
#'
#' @param date A `Date` object representing the target date.
#' @param calendar A vector of `Date` objects representing valid business days.
#'
#' @return A `Date` object corresponding to the last business date before or equal to `date`.
#' @examples
#' calendar <- as.Date(c("2024-01-02", "2024-01-03", "2024-01-04"))
#' .getLastBizDate(as.Date("2024-01-05"), calendar) # Returns "2024-01-04"
#'
#' @export
.getLastBizDate <- function(date, calendar) {
  while (!(date %in% calendar) && date > min(calendar)) {
    date <- date - days(1)
  }
  return(date)
}
