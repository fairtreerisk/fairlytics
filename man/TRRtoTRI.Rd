% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trr_to_tri.R
\name{TRRtoTRI}
\alias{TRRtoTRI}
\title{TRRtoTRI}
\usage{
TRRtoTRI(dfTRR, FocusAt = "Start")
}
\arguments{
\item{dfTRR}{TRR dataframe (columns: date, Ticker, TR_Index)}

\item{FocusAt}{character string. Should the index of 1 be set at the 'Start' or the 'End' , Default: 'Start'}
}
\value{
TRI dataframe
}
\description{
Converts a TRR dataframe to a TRI dataframe, starting with an index of 1
}
