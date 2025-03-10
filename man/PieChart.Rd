% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pie_chart.R
\name{PieChart}
\alias{PieChart}
\title{Create a Static Pie Chart with Customizable Gaps using ggplot2}
\usage{
PieChart(
  df,
  value_col,
  color_col,
  chart_title = NULL,
  gap_size = 1,
  gap_color = "white"
)
}
\arguments{
\item{df}{A data frame containing the data to be visualized.}

\item{value_col}{A string specifying the column in \code{df} that contains numerical values for the chart.}

\item{color_col}{A string specifying the column in \code{df} that determines the fill color of the pie chart.}

\item{chart_title}{A string specifying the title of the chart (default: \code{NULL}).}

\item{gap_size}{A numeric value specifying the size of the gap between pie slices (default: \code{1}).}

\item{gap_color}{A string specifying the color of the gap (default: \code{"white"}).}
}
\value{
A ggplot object representing the pie chart.
}
\description{
This function generates a pie chart with customizable gaps between segments using ggplot2. The chart is based on the specified value and color columns from a data frame.
}
\examples{
library(ggplot2)
test_data <- data.frame(
  category = c("A", "B", "C"),
  values = c(0.3, 0.5, 0.2)
)
PieChart(test_data, value_col = "values", color_col = "category", chart_title = "Example Pie Chart")
}
