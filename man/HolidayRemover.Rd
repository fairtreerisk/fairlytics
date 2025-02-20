% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/holiday_remover.R
\name{HolidayRemover}
\alias{HolidayRemover}
\title{HolidayRemover}
\usage{
HolidayRemover(data, date_col, calendar_name = "SouthAfrica")
}
\arguments{
\item{data}{A data frame containing a \code{Date} column.
This column is required and must represent the dates to evaluate.}

\item{date_col}{A string specifying the name of the column in \code{data} that contains the date values.
The column should be of class \code{Date} or convertible to it.}

\item{calendar_name}{A string specifying the name of the holiday calendar to use.
The default is \code{"SouthAfrica"}.
You can choose any valid calendar supported by the \code{bizdays} package.}
}
\value{
data-frame excluding holidays and weekends based on the calendar provided
}
\description{
The \code{HolidayRemover} function cleanses a dataset by removing weekends and holidays.
It generates a custom business day calendar based on the minimum and maximum dates in the dataset,
using a specified holiday calendar. This is particularly useful for analyzing time series data
that should only include business days.
}
\examples{
# Example dataset
data_df <- data.frame(
  Date = seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by = "day"),
  Value = rnorm(10)
)

# Removing weekends and South African holidays from the dataset
cleaned_data <- HolidayRemover(data = data_df, "Date", calendar_name = "SouthAfrica")
print(cleaned_data)
}
