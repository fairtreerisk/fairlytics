% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/donut_plotly.R
\name{DonutPlotly}
\alias{DonutPlotly}
\title{Create an Interactive Donut Chart using Plotly}
\usage{
DonutPlotly(df, value_col, label_col, chart_title = NULL, space_size = 0.65)
}
\arguments{
\item{df}{A data frame containing the data to be visualized.}

\item{value_col}{A string specifying the column in \code{df} that contains numerical values for the chart.}

\item{label_col}{A string specifying the column in \code{df} that contains labels for the chart.}

\item{chart_title}{A string specifying the title of the chart (default: \code{NULL}).}

\item{space_size}{A numeric value specifying the size of the hole in the donut chart (default: \code{0.65}).}
}
\value{
A Plotly object representing the donut chart.
}
\description{
This function generates a donut chart using Plotly, based on the specified value and label columns from a data frame.
}
\examples{
# do not run
library(plotly)
test_df <- data.frame(
  category = c("A", "B", "C"),
  values = c(30, 50, 20)
)
DonutPlotly(test_df, value_col = "values", label_col = "category", chart_title = "Example Donut Chart")
}
